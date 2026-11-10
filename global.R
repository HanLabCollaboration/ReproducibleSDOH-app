# global.R — index registry and helpers shared by ui.R and server.R
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(sf)
library(DT)
library(ReproducibleSDOH)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Serve www/ under "assets/". Shiny serves www/ on its own only when the app is
# started with runApp(<folder>); when app.R is sourced or run from an IDE, the
# files would otherwise not be found.
addResourcePath("assets", normalizePath("www"))

# =============================================================================
# Index registry
# =============================================================================
# Everything the app knows about an index lives here. To add an index, add one
# entry: the UI choices, the data calls and the map styling all read from it.
#
#   label      name shown in the UI
#   fun        the ReproducibleSDOH get*() function
#   args       extra arguments for it (e.g. the NDI type)
#   has_scope  FALSE when the index has no national / state scope
#   geos       geographies the index is published for
#   measures   one entry per score: label, raw column, best column
#   higher     what a higher value means; "worse" reverses the palette, so the
#              dark end of the scale is always the more disadvantaged one
#   domain     fixed colour-scale limits, or NULL to use the 1st-99th
#              percentiles of all areas the index was computed against
#   about      one-sentence description for the About tab

one_measure <- function(label, raw, best) {
  stats::setNames(list(list(label = label, raw = raw, best = best)), raw)
}

ICE_LABELS <- c(
  ICE_inc    = "Income",
  ICE_edu    = "Education",
  ICE_rewb   = "Race/ethnicity",
  ICE_wbinc  = "Race and income",
  ICE_wpcinc = "Income among White households"
)

INDICES <- list(
  yost = list(
    label = "Yost Index", fun = getYostIndex, args = list(), has_scope = TRUE,
    geos = c("tract", "block group"),
    measures = one_measure("Yost Index", "Yost", "YostStabilizedImputed"),
    higher = "better", higher_text = "higher = higher socioeconomic status",
    domain = c(8, 12),
    about = "A composite measure of neighborhood socioeconomic status. The best version stabilizes small-area estimates and imputes missing components from neighboring areas."
  ),
  ndi_messer = list(
    label = "NDI Messer", fun = getNDI, args = list(type = "messer"), has_scope = TRUE,
    geos = c("tract", "block group"),
    measures = one_measure("NDI Messer", "NDI", "NDIImputed"),
    higher = "worse", higher_text = "higher = more deprived",
    domain = NULL,
    about = "The Neighborhood Deprivation Index by the Messer method. The best version imputes missing components from neighboring areas. Shown for census tracts only, because some of its ACS tables are not available for block groups."
  ),
  ndi_powell_wiley = list(
    label = "NDI Powell-Wiley", fun = getNDI, args = list(type = "powell_wiley"),
    has_scope = TRUE,
    geos = c("tract", "block group"),
    measures = one_measure("NDI Powell-Wiley", "NDI", "NDIImputed"),
    higher = "worse", higher_text = "higher = more deprived",
    domain = NULL,
    about = "The Neighborhood Deprivation Index by the Powell-Wiley method. Published for census tracts only, because some of its ACS tables are not available for block groups."
  ),
  ice = list(
    label = "ICE", fun = getICE, args = list(), has_scope = FALSE,
    geos = c("tract", "block group"),
    measures = lapply(stats::setNames(names(ICE_LABELS), names(ICE_LABELS)), function(m) {
      list(label = paste0("ICE: ", ICE_LABELS[[m]]), raw = m, best = paste0(m, "Imputed"))
    }),
    higher = "better", higher_text = "-1 = most deprived, +1 = most privileged",
    domain = c(-1, 1),
    about = "The Index of Concentration at the Extremes: five measures of how concentrated an area is at the privileged or the deprived extreme. It has no scope, because each value depends only on the area itself."
  )
)

INDEX_CHOICES   <- stats::setNames(names(INDICES), vapply(INDICES, `[[`, "", "label"))

# Every score of every index, as "<index>::<measure>": the choices of the
# Index vs. Index tab.
PAIR_CHOICES <- unlist(lapply(names(INDICES), function(index) {
  measures <- INDICES[[index]]$measures
  stats::setNames(paste0(index, "::", names(measures)),
                  vapply(measures, `[[`, "", "label"))
}))
MEASURE_CHOICES <- stats::setNames(names(ICE_LABELS), ICE_LABELS)

# The score column for an index, measure and version ("raw" or "best").
score_column <- function(spec, measure, version) {
  m <- spec$measures[[measure]] %||% spec$measures[[1]]
  m[[version]]
}
score_label <- function(spec, measure) (spec$measures[[measure]] %||% spec$measures[[1]])$label
score_columns <- function(spec) unlist(lapply(spec$measures, function(m) c(m$raw, m$best)), use.names = FALSE)

# =============================================================================
# Coverage
# =============================================================================

# Boundaries for 2011 and 2012 tracts are full-detail files of about 520 MB
# each, too heavy to map from, so the app starts in 2013 for both geographies.
YEAR_MIN <- 2013L
YEAR_MAX <- 2024L

# Boundaries shipped with the app (see data-raw/), at full resolution, so that
# the first map needs no boundary download.
STARTUP_YEAR   <- 2024L
BOUNDARIES_DIR <- file.path("data", sprintf("boundaries_%d", STARTUP_YEAR))

STATE_FIPS <- c(
  AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08", CT = "09",
  DE = "10", DC = "11", FL = "12", GA = "13", HI = "15", ID = "16", IL = "17",
  IN = "18", IA = "19", KS = "20", KY = "21", LA = "22", ME = "23", MD = "24",
  MA = "25", MI = "26", MN = "27", MS = "28", MO = "29", MT = "30", NE = "31",
  NV = "32", NH = "33", NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38",
  OH = "39", OK = "40", OR = "41", PA = "42", RI = "44", SC = "45", SD = "46",
  TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53", WV = "54",
  WI = "55", WY = "56"
)
STATE_CHOICES  <- sort(names(STATE_FIPS))
DEFAULT_STATE  <- "CA"
DEFAULT_COUNTY <- "06085"   # Santa Clara County
ALL_COUNTIES   <- "__ALL__"

# =============================================================================
# Loading data
# =============================================================================
# Everything comes from ReproducibleSDOH, which caches downloads on disk. On
# top of that, the most recent results are kept in memory, shared by every
# session of the R process.

memo <- function(cache, key, value, max_size) {
  if (!is.null(cache[[key]])) return(cache[[key]])
  keys <- ls(cache)
  if (length(keys) >= max_size) rm(list = keys[1], envir = cache)
  cache[[key]] <- value   # `value` is a promise: only evaluated on a miss
  cache[[key]]
}
SCORES_CACHE     <- new.env(parent = emptyenv())
BOUNDARIES_CACHE <- new.env(parent = emptyenv())
COUNTIES_CACHE   <- new.env(parent = emptyenv())

# Quintile (1 = lowest values) of each value among the non-missing ones.
to_quintile <- function(x) {
  n <- sum(!is.na(x))
  if (n < 5) return(rep(NA_integer_, length(x)))
  as.integer(cut(rank(x, na.last = "keep") / n, breaks = seq(0, 1, 0.2), include.lowest = TRUE))
}

# One index for every area of the country: GEOID, NAME, tot_pop, both versions
# of each score, and a `<score>Quintile` column for each.
# Areas with no population have no score, in either version.
# Quintiles follow the scope: among all areas of the country for national
# scope (and for ICE), within each state for state scope.
load_scores <- function(index, geo, year, scope) {
  spec <- INDICES[[index]]
  if (!spec$has_scope) scope <- NULL
  key <- paste(index, geo, year, scope %||% "none", sep = "|")
  memo(SCORES_CACHE, key, max_size = 6, value = {
    args <- c(spec$args, list(geo = geo, year = year, version = "both", info = TRUE, quiet = TRUE))
    if (spec$has_scope) args$scope <- scope
    d <- as.data.frame(do.call(spec$fun, args))
    by_state <- identical(scope, "state")
    # An area with no population has no score. The published imputed ICE
    # measures do give some of them one (filled in from neighbors); drop it.
    no_pop <- !is.na(d$tot_pop) & d$tot_pop == 0
    for (col in score_columns(spec)) {
      d[[col]][no_pop] <- NA
      d[[paste0(col, "Quintile")]] <-
        if (by_state) stats::ave(d[[col]], substr(d$GEOID, 1, 2), FUN = to_quintile)
        else to_quintile(d[[col]])
    }
    d
  })
}

has_startup_boundaries <- function(geo, year, fips) {
  year == STARTUP_YEAR &&
    file.exists(file.path(BOUNDARIES_DIR, gsub(" ", "_", geo), paste0(fips, ".rds")))
}

# Boundaries of one state, at full resolution: an sf object with GEOID, in
# EPSG:4326. Uses the file shipped with the app when there is one; otherwise
# downloads the boundaries through getGeoInfo().
load_boundaries <- function(geo, year, fips) {
  key <- paste(geo, year, fips, sep = "|")
  memo(BOUNDARIES_CACHE, key, max_size = 8, value = {
    if (has_startup_boundaries(geo, year, fips)) {
      readRDS(file.path(BOUNDARIES_DIR, gsub(" ", "_", geo), paste0(fips, ".rds")))
    } else {
      g <- getGeoInfo(geo = geo, year = year, states = fips, geometry = TRUE, quiet = TRUE)
      g <- g[!sf::st_is_empty(g), "GEOID"]
      g <- sf::st_make_valid(sf::st_transform(g, 4326))
      g[!sf::st_is_empty(g), ]
    }
  })
}

# Thins full-resolution boundaries for whole-state maps (tolerance in degrees,
# about 50 m), where thousands of detailed polygons would make the map slow.
thin_boundaries <- function(g) {
  s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(s2))
  out <- suppressWarnings(suppressMessages(
    sf::st_simplify(g, preserveTopology = TRUE, dTolerance = 0.0005)
  ))
  out[!sf::st_is_empty(out), ]
}

# Counties of one state, read from the area names: a named vector of 5-digit
# county GEOIDs. Names look like "Census Tract 4001; Alameda County; California"
# (separated by commas before 2022).
load_counties <- function(geo, year, fips) {
  key <- paste(geo, year, fips, sep = "|")
  memo(COUNTIES_CACHE, key, max_size = 60, value = {
    gi <- getGeoInfo(geo = geo, year = year, states = fips, quiet = TRUE)
    first  <- !duplicated(substr(gi$GEOID, 1, 5))
    parts  <- strsplit(gi$NAME[first], "[;,] *")
    county <- vapply(parts, function(p) if (length(p) >= 2) p[length(p) - 1] else p[1], "")
    out <- stats::setNames(substr(gi$GEOID[first], 1, 5), county)
    out[order(names(out))]
  })
}

# =============================================================================
# Map styling
# =============================================================================

NA_COLOR <- "#CCCCCC"

# Light basemap without labels.
# With a CARTO basemaps key in the environment variable CARTO_API_KEY (set it in
# .Renviron, see README), the CARTO Positron tiles; without one, Esri's light
# gray canvas, which needs no key.
add_basemap <- function(map) {
  key <- Sys.getenv("CARTO_API_KEY")
  if (!nzchar(key)) return(addProviderTiles(map, providers$Esri.WorldGrayCanvas))
  addTiles(
    map,
    urlTemplate = paste0(
      "https://basemaps.cartocdn.com/rastertiles/light_nolabels/{z}/{x}/{y}{r}.png?key=", key),
    attribution = paste(
      "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors,",
      "&copy; <a href='https://carto.com/attributions'>CARTO</a>")
  )
}

fmt_int <- function(x) ifelse(is.na(x), "N/A", format(x, big.mark = ",", trim = TRUE))
fmt_num <- function(x) ifelse(is.na(x), "N/A", formatC(x, format = "f", digits = 2))

# Limits of the numerical colour scale of a score: the index's fixed limits, or
# the 1st-99th percentiles of `ref`, the values of every area the index was
# computed against.
index_domain <- function(spec, ref) {
  if (!is.null(spec$domain)) return(spec$domain)
  domain <- unname(stats::quantile(ref, c(0.01, 0.99), na.rm = TRUE))
  if (anyNA(domain) || domain[1] == domain[2]) c(0, 1) else domain
}

# How to colour a set of areas by a score. Returns the fill colours and what
# addLegend() needs: list(fill, pal, legend_values, title).
#   d        the areas on the map
#   col      the score column
#   domain   limits of the numerical scale, from index_domain()
#   display  "numerical" or "quintile"
#   title    legend title, the column name by default
style_index <- function(d, col, spec, domain, display, title = col) {
  reverse <- spec$higher == "worse"
  note    <- paste0("<br/><span style='font-weight:400; font-size:11px;'>", spec$higher_text, "</span>")
  if (identical(display, "quintile")) {
    pal <- colorFactor(viridisLite::viridis(5, direction = if (reverse) -1 else 1),
                       domain = factor(1:5), na.color = NA_COLOR)
    return(list(fill = pal(factor(d[[paste0(col, "Quintile")]], levels = 1:5)), pal = pal,
                legend_values = factor(1:5), title = paste0(title, " quintile", note)))
  }
  pal <- colorNumeric("viridis", domain = domain, reverse = reverse, na.color = NA_COLOR)
  # Values beyond the limits take the colour of the nearest limit.
  list(fill = pal(pmin(pmax(d[[col]], domain[1]), domain[2])), pal = pal,
       legend_values = domain, title = paste0(title, note))
}

# Hover labels: name, population, and each (label = column) score in `scores`
# with its quintile.
area_labels <- function(d, scores) {
  lines <- vapply(names(scores), function(lbl) {
    col <- scores[[lbl]]
    q   <- d[[paste0(col, "Quintile")]]
    paste0("<span style='color:#555;'>", lbl, ":</span> ", fmt_num(d[[col]]),
           ifelse(is.na(q), "", paste0(" (quintile ", q, ")")), "<br/>")
  }, character(nrow(d)))
  lines <- if (is.matrix(lines)) apply(lines, 1, paste0, collapse = "") else paste0(lines, collapse = "")
  lapply(paste0(
    "<div style='font-size:13px; line-height:1.6;'>",
    "<strong>", htmltools::htmlEscape(d$NAME), "</strong><br/>",
    "<span style='color:#555;'>Population:</span> ", fmt_int(d$tot_pop), "<br/>",
    lines, "</div>"
  ), htmltools::HTML)
}
