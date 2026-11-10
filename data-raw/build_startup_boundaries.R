# data-raw/build_startup_boundaries.R
# One-shot script: builds the boundaries the app ships with, so the first map
# appears without a download.
#
# - Reads the full-resolution boundaries with ReproducibleSDOH::getGeoInfo().
# - Writes one file per state: data/boundaries_<year>/<geo>/<state fips>.rds
#   (GEOID + geometry, EPSG:4326, xz compressed), so the app only reads the
#   state it shows.
#
# Only boundaries are stored. Index scores, names and population always come
# from the get*() functions at run time.
#
# Run from the app folder:  Rscript data-raw/build_startup_boundaries.R

suppressPackageStartupMessages({
  library(sf)
  library(ReproducibleSDOH)
})

YEAR    <- 2024L
GEOS    <- c("tract", "block group")
OUT_DIR <- file.path("data", sprintf("boundaries_%d", YEAR))

for (geo in GEOS) {
  geo_tag <- gsub(" ", "_", geo)
  cat(sprintf("[%s %d] read ...", geo, YEAR)); t0 <- Sys.time()

  # The same preparation load_boundaries() applies to a downloaded year.
  g <- getGeoInfo(geo = geo, year = YEAR, geometry = TRUE, quiet = TRUE)
  g <- g[!sf::st_is_empty(g), "GEOID"]
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  g <- g[!sf::st_is_empty(g), ]

  cat(" write ...")
  dir <- file.path(OUT_DIR, geo_tag)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  for (piece in split(g, substr(g$GEOID, 1, 2))) {
    piece <- piece[order(piece$GEOID), ]
    saveRDS(piece, file.path(dir, paste0(substr(piece$GEOID[1], 1, 2), ".rds")),
            compress = "xz")
  }

  files <- list.files(dir, full.names = TRUE)
  cat(sprintf(" %d areas, %d files, %.1f MB  (%.0fs)\n", nrow(g), length(files),
              sum(file.size(files)) / 1024^2,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat("Done.\n")
