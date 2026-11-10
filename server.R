# server.R

server <- function(input, output, session) {

  # Runs a data call; on failure shows the error and stops whatever needed it.
  safely <- function(expr) {
    tryCatch(expr, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
  }

  # ── Selection ────────────────────────────────────────────────────────────────

  spec    <- reactive(INDICES[[input$index]])

  # One side of the Index vs. Index tab: its index, measure and score column.
  vs_side <- function(id) reactive({
    parts <- strsplit(req(input[[id]]), "::", fixed = TRUE)[[1]]
    side_spec <- INDICES[[parts[1]]]
    list(index = parts[1], spec = side_spec, measure = parts[2],
         col   = score_column(side_spec, parts[2], input$version),
         label = score_label(side_spec, parts[2]))
  })
  vs_a <- vs_side("vs_a")
  vs_b <- vs_side("vs_b")

  # Geographies every index on the current tab is published for.
  allowed_geos <- reactive({
    if (identical(input$nav, "tab_vs")) intersect(vs_a()$spec$geos, vs_b()$spec$geos)
    else spec()$geos
  })
  geo     <- reactive(if (input$geo %in% allowed_geos()) input$geo else allowed_geos()[1])
  year    <- debounce(reactive(as.integer(input$year)), 400)
  scope   <- reactive(if (spec()$has_scope) input$scope else NULL)
  fips    <- reactive(STATE_FIPS[[req(input$state)]])
  measure <- reactive(if (input$index == "ice") input$measure else names(spec()$measures)[1])

  # Geographies an index is not published for cannot be picked.
  observeEvent(allowed_geos(), {
    all_geos <- c("tract", "block group")
    updateRadioGroupButtons(session, "geo", selected = geo(),
                            disabledChoices = setdiff(all_geos, allowed_geos()))
  })

  counties <- reactive(req(safely(load_counties(geo(), year(), fips()))))

  # Keep the county when it is still in the list (same state, another year or
  # geography); otherwise start from the default or the first county.
  observeEvent(counties(), {
    choices  <- c(stats::setNames(ALL_COUNTIES, paste("All counties in", input$state)), counties())
    current  <- isolate(input$county)
    selected <- if (!is.null(current) && current %in% choices) current
                else if (DEFAULT_COUNTY %in% choices) DEFAULT_COUNTY
                else counties()[[1]]
    updatePickerInput(session, "county", choices = choices, selected = selected)
  })

  # The county, once the picker has caught up with the selected state.
  county <- reactive({
    value <- req(input$county)
    req(value == ALL_COUNTIES || value %in% counties())
    value
  })
  whole_state <- reactive(county() == ALL_COUNTIES)

  # ── Data ─────────────────────────────────────────────────────────────────────

  # The index for the whole country: scores, quintiles, names and population.
  scores <- reactive(req(safely(load_scores(input$index, geo(), year(), scope()))))

  # The selected areas, without boundaries.
  area <- reactive({
    d <- scores()
    d <- d[substr(d$GEOID, 1, 2) == fips(), , drop = FALSE]
    if (!whole_state()) d <- d[substr(d$GEOID, 1, 5) == county(), , drop = FALSE]
    rownames(d) <- NULL
    d
  })

  # Their boundaries. Depends on the area only, not on the index.
  area_boundaries <- reactive({
    g <- req(safely(load_boundaries(geo(), year(), fips())))
    # A county is drawn at full resolution; a whole state is thinned.
    if (whole_state()) thin_boundaries(g) else g[substr(g$GEOID, 1, 5) == county(), ]
  })

  map_sf <- reactive({
    g <- area_boundaries()
    d <- area()
    cols <- setdiff(names(d), "GEOID")
    cbind(g, d[match(g$GEOID, d$GEOID), cols, drop = FALSE])
  })

  score_col <- reactive(score_column(spec(), measure(), input$version))
  domain    <- reactive({
    index_domain(spec(), scores()[[score_column(spec(), measure(), "best")]])
  })

  area_name <- reactive({
    where <- if (whole_state()) input$state
             else paste0(names(counties())[match(county(), counties())], ", ", input$state)
    paste0(if (geo() == "tract") "census tracts" else "block groups", " in ", where)
  })

  # "national scope" / "state scope"; empty for an index without scope.
  scope_text <- reactive(if (is.null(scope())) "" else paste0(", ", scope(), " scope"))

  # ── Tab 1: map ───────────────────────────────────────────────────────────────

  # Base map (rendered once; proxy updates it)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = TRUE, minZoom = 2, maxZoom = 18,
      preferCanvas = TRUE   # much faster than SVG for whole-state maps
    )) |>
      add_basemap() |>
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })

  area_bounds <- function(g) {
    # Alaska crosses the 180th meridian, so its bounding box spans the globe.
    if (substr(g$GEOID[1], 1, 2) == "02" && nrow(g) > 100)
      return(c(xmin = -170, ymin = 51, xmax = -130, ymax = 72))
    sf::st_bbox(g)
  }

  # Move to the selected area when it changes
  observeEvent(area_boundaries(), {
    g <- area_boundaries()
    req(nrow(g) > 0)
    bb <- area_bounds(g)
    leafletProxy("map") |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })

  # Redraw polygons/legend when the data or any display choice changes
  observe({
    m   <- map_sf()
    col <- score_col()
    req(nrow(m) > 0, col %in% names(m))

    style <- style_index(m, col, spec(), domain(), input$display)

    leafletProxy("map") |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        data        = m,
        fillColor   = style$fill,
        fillOpacity = 0.6,
        color       = "#444444", weight = 0.4, smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          weight = 2, color = "#222222", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label        = area_labels(m, stats::setNames(col, score_label(spec(), measure()))),
        labelOptions = labelOptions(
          style     = list("font-weight" = "500", "font-size" = "13px",
                           "padding" = "4px 8px"),
          direction = "auto"
        )
      ) |>
      addLegend("bottomright", pal = style$pal, values = style$legend_values,
                title = style$title, opacity = 0.9)
  })

  # What is on the map: number of areas, population, and areas without a score
  output$map_note <- renderUI({
    d   <- area()
    col <- score_col()
    n_missing <- sum(is.na(d[[col]]))

    if (nrow(d) > 0 && n_missing == nrow(d)) {
      return(div(class = "map-note map-note--warning",
        tags$i(class = "fa fa-triangle-exclamation", style = "margin-right: 6px; color: #f0ad4e;"),
        sprintf("None of the %s has a %s score (%s) for %d%s.",
                area_name(), spec()$label, col, year(), scope_text())))
    }
    div(class = "map-note",
      tags$i(class = "fa fa-circle-info", style = "margin-right: 6px; color: #95a5a6;"),
      sprintf("%s %s, %d%s: total population %s. %s without a %s score (shown in grey).",
              fmt_int(nrow(d)), area_name(), year(), scope_text(),
              fmt_int(sum(as.numeric(d$tot_pop), na.rm = TRUE)),
              fmt_int(n_missing), col))
  })

  # ── Table ────────────────────────────────────────────────────────────────────

  output$table_title <- renderText({
    paste0(spec()$label, ": ", area_name(), ", ", year(), scope_text())
  })

  output$table <- renderDT({
    tbl <- area()
    is_score <- vapply(tbl, is.double, logical(1))
    tbl[is_score] <- lapply(tbl[is_score], round, 3)
    datatable(tbl, filter = "top", rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE,
                             columnDefs = list(list(targets = "_all",
                                                    className = "dt-nowrap"))))
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(paste(c(input$index, gsub(" ", "_", geo()), scope(), year(), input$state,
                     if (!whole_state()) county()), collapse = "_"), ".csv")
    },
    content = function(file) utils::write.csv(area(), file, row.names = FALSE)
  )

  # ── Tab 2: raw vs. best ──────────────────────────────────────────────────────

  make_cmp_title <- function(version, label) {
    renderUI({
      d   <- area()
      col <- score_column(spec(), measure(), version)
      tagList(label, " ", tags$span(sprintf(
        "%s: %s of %s areas without a score", col,
        fmt_int(sum(is.na(d[[col]]))), fmt_int(nrow(d)))))
    })
  }

  make_cmp_map <- function(version) {
    renderLeaflet({
      m   <- map_sf()
      col <- score_column(spec(), measure(), version)
      req(nrow(m) > 0, col %in% names(m))

      # Both maps share one colour scale.
      style <- style_index(m, col, spec(), domain(), input$display)
      bb    <- area_bounds(m)

      leaflet(m, options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) |>
        add_basemap() |>
        addPolygons(
          fillColor   = style$fill,
          fillOpacity = 0.6,
          color = "#444", weight = 0.4, smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            weight = 2, fillOpacity = 0.9, bringToFront = TRUE
          ),
          label = area_labels(m, c(
            Raw  = score_column(spec(), measure(), "raw"),
            Best = score_column(spec(), measure(), "best")
          )),
          labelOptions = labelOptions(
            style = list("font-size" = "12px", "padding" = "3px 6px"),
            direction = "auto"
          )
        ) |>
        addLegend("bottomright", pal = style$pal, values = style$legend_values,
                  title = style$title, opacity = 0.9) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
  }

  output$cmp_title_raw  <- make_cmp_title("raw",  "Raw")
  output$cmp_title_best <- make_cmp_title("best", "Best")
  output$cmp_map_raw    <- make_cmp_map("raw")
  output$cmp_map_best   <- make_cmp_map("best")

  # ── Tab 3: index vs. index ───────────────────────────────────────────────────

  vs_scores <- function(side) reactive({
    s <- side()
    req(safely(load_scores(s$index, geo(), year(), if (s$spec$has_scope) input$scope)))
  })
  vs_scores_a <- vs_scores(vs_a)
  vs_scores_b <- vs_scores(vs_b)

  # The selected areas with both scores, as columns A and B (two indices can
  # share a column name, such as NDI), and their quintiles.
  vs_sf <- reactive({
    g  <- area_boundaries()
    da <- vs_scores_a(); ia <- match(g$GEOID, da$GEOID)
    db <- vs_scores_b(); ib <- match(g$GEOID, db$GEOID)
    g$NAME      <- da$NAME[ia]
    g$tot_pop   <- da$tot_pop[ia]
    g$A         <- da[[vs_a()$col]][ia]
    g$AQuintile <- da[[paste0(vs_a()$col, "Quintile")]][ia]
    g$B         <- db[[vs_b()$col]][ib]
    g$BQuintile <- db[[paste0(vs_b()$col, "Quintile")]][ib]
    g
  })

  # How the two agree over the mapped areas
  output$vs_note <- renderUI({
    m  <- sf::st_drop_geometry(vs_sf())
    ok <- !is.na(m$A) & !is.na(m$B)
    rho <- if (sum(ok) > 2) suppressWarnings(stats::cor(m$A[ok], m$B[ok], method = "spearman")) else NA
    div(class = "map-note",
      tags$i(class = "fa fa-circle-info", style = "margin-right: 6px; color: #95a5a6;"),
      sprintf("%s %s, %d. %s have both scores; their Spearman rank correlation is %s. ",
              fmt_int(nrow(m)), area_name(), year(), fmt_int(sum(ok)), fmt_num(rho)),
      "Each map has its own colour scale; the dark end is always the more disadvantaged.")
  })

  make_vs_title <- function(side, col) {
    renderUI({
      s <- side()
      m <- vs_sf()
      tagList(s$label, " ", tags$span(sprintf(
        "%s%s: %s of %s areas without a score", s$col,
        if (s$spec$has_scope) paste0(", ", input$scope, " scope") else "",
        fmt_int(sum(is.na(m[[col]]))), fmt_int(nrow(m)))))
    })
  }

  make_vs_map <- function(side, scores, col) {
    renderLeaflet({
      s <- side()
      m <- vs_sf()
      req(nrow(m) > 0)

      domain <- index_domain(s$spec, scores()[[score_column(s$spec, s$measure, "best")]])
      style  <- style_index(m, col, s$spec, domain, input$display, title = s$col)
      bb     <- area_bounds(m)

      leaflet(m, options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) |>
        add_basemap() |>
        addPolygons(
          fillColor   = style$fill,
          fillOpacity = 0.6,
          color = "#444", weight = 0.4, smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            weight = 2, fillOpacity = 0.9, bringToFront = TRUE
          ),
          label = area_labels(m, stats::setNames(c("A", "B"), c(vs_a()$label, vs_b()$label))),
          labelOptions = labelOptions(
            style = list("font-size" = "12px", "padding" = "3px 6px"),
            direction = "auto"
          )
        ) |>
        addLegend("bottomright", pal = style$pal, values = style$legend_values,
                  title = style$title, opacity = 0.9) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
  }

  output$vs_title_a <- make_vs_title(vs_a, "A")
  output$vs_title_b <- make_vs_title(vs_b, "B")
  output$vs_map_a   <- make_vs_map(vs_a, vs_scores_a, "A")
  output$vs_map_b   <- make_vs_map(vs_b, vs_scores_b, "B")
}
