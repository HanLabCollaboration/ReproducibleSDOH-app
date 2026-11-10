# ui.R

info_tip <- function(html) {
  span(class = "info-icon", icon("circle-info"), span(class = "info-tip", HTML(html)))
}

loading_overlay <- div(
  class = "loading-overlay",
  tags$i(class = "fa fa-circle-notch fa-spin"),
  "Loading... the first use of a year downloads its files, which can take up to a minute."
)

# ── Controls, shared by the map tabs ──────────────────────────────────────────
control_panel <- div(class = "control-panel",

  # Row 1: Index | Version | Display | Scope
  fluidRow(
    column(5,
      conditionalPanel(
        condition = "input.nav != 'tab_vs'",
        radioGroupButtons(
          "index", "Index",
          choices = INDEX_CHOICES, selected = "yost",
          status = "primary", size = "sm", justified = TRUE
        )
      ),
      # The Index vs. Index tab picks two indices instead of one.
      conditionalPanel(
        condition = "input.nav == 'tab_vs'",
        fluidRow(
          column(6, pickerInput(
            "vs_a", "Index, left map",
            choices = PAIR_CHOICES, selected = "yost::Yost", width = "100%",
            options = pickerOptions(style = "btn-outline-secondary")
          )),
          column(6, pickerInput(
            "vs_b", "Index, right map",
            choices = PAIR_CHOICES, selected = "ndi_powell_wiley::NDI", width = "100%",
            options = pickerOptions(style = "btn-outline-secondary")
          ))
        )
      )
    ),
    column(2,
      radioGroupButtons(
        "version",
        tagList("Version", info_tip(
          "<b>Raw:</b> computed from the ACS estimates as published. Areas with a missing component have no score.<br><br>
           <b>Best:</b> missing components are imputed from neighboring areas (and, for the Yost Index, small-area estimates are stabilized first), so far fewer areas are missing."
        )),
        choiceNames = c("Raw", "Best"), choiceValues = c("raw", "best"),
        selected = "best",
        status = "primary", size = "sm", justified = TRUE
      )
    ),
    column(3,
      radioGroupButtons(
        "display",
        tagList("Display", info_tip(
          "<b>Quintile:</b> 1 holds the fifth of areas with the lowest values, 5 the fifth with the highest.<br><br>
           Quintiles follow the scope: among all areas of the country for national scope (and for ICE), within each state for state scope."
        )),
        choiceNames = c("Numerical", "Quintile"), choiceValues = c("numerical", "quintile"),
        selected = "numerical",
        status = "primary", size = "sm", justified = TRUE
      )
    ),
    column(2,
      # Hidden when nothing on the tab has a scope (ICE has none).
      conditionalPanel(
        condition = "input.nav == 'tab_vs'
                       ? !(input.vs_a.indexOf('ice') == 0 && input.vs_b.indexOf('ice') == 0)
                       : input.index != 'ice'",
        radioGroupButtons(
          "scope",
          tagList("Scope", info_tip(
            "<b>National:</b> the index compares every area with the whole country.<br><br>
             <b>State:</b> the index is computed separately within each state, so values are comparable only inside a state."
          )),
          choices = c("national", "state"), selected = "national",
          status = "primary", size = "sm", justified = TRUE
        )
      )
    )
  ),

  # Row 2: ICE measure (only for ICE)
  conditionalPanel(
    condition = "input.index == 'ice' && input.nav != 'tab_vs'",
    fluidRow(column(12,
      radioGroupButtons(
        "measure", "ICE measure",
        choices = MEASURE_CHOICES, selected = MEASURE_CHOICES[[1]],
        status = "primary", size = "sm", justified = TRUE
      )
    ))
  ),

  # Row 3: State | County | Geography | Year. State + County sit under Index,
  # Geography under Version, so the columns of the two rows line up.
  fluidRow(
    column(2,
      pickerInput(
        "state", "State",
        choices = STATE_CHOICES, selected = DEFAULT_STATE, multiple = FALSE, width = "100%",
        options = pickerOptions(liveSearch = TRUE, style = "btn-outline-secondary")
      )
    ),
    column(3,
      pickerInput(
        "county",
        tagList("County", info_tip(
          "Pick a single county to map its tracts / block groups, or choose <b>All counties</b> (top of the list) to map the whole state.<br><br>
           <i>Whole-state maps can be slow, especially for block groups.</i>"
        )),
        choices = NULL, multiple = FALSE, width = "100%",
        options = pickerOptions(liveSearch = TRUE, style = "btn-outline-secondary",
                                noneSelectedText = "Select a county")
      )
    ),
    column(2,
      radioGroupButtons(
        "geo", "Geography",
        choices = c("tract", "block group"), selected = "tract",
        status = "primary", size = "sm", justified = TRUE
      )
    ),
    column(5,
      sliderInput(
        "year",
        tagList("ACS 5-year", info_tip(sprintf(
          "The selected year is the last year of a 5-year ACS period: 2022 is the 2018&ndash;2022 survey.<br><br>
           Boundaries for %d are built into the app. Other years are downloaded the first time they are used, which may take a moment.",
          STARTUP_YEAR
        ))),
        min = YEAR_MIN, max = YEAR_MAX, value = STARTUP_YEAR, step = 1, sep = "", ticks = FALSE
      )
    )
  )
)

navbarPage(
  title = div(
    span("ReproducibleSDOH", style = "font-weight: 700; color: #fff;"),
  ),
  theme = shinytheme("flatly"),
  id    = "nav",

  header = tagList(
    tags$head(
      tags$style(HTML("
        #sticky-footer {
          position: fixed;
          bottom: 0; left: 0;
          width: 100%;
          border-top: 1px solid #dee2e6;
          color: #6c757d;
          z-index: 1000;
          background-color: #f8f9fa;
          padding: 12px 20px;
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
        }
        body { padding-bottom: 90px; }

        .tab-content { padding-top: 4px; }

        .control-panel {
          background-color: #ffffff;
          border: 1px solid #e3e8ee;
          border-radius: 10px;
          padding: 18px 22px 10px 22px;
          margin: 4px 15px 18px 15px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.04);
        }
        /* Every label is one line of the same height, with or without an
           info icon, so the controls of a row start at the same level. */
        .control-panel .control-label {
          display: block;
          font-weight: 600;
          color: #34495e;
          font-size: 13px;
          line-height: 20px;
          height: 20px;
          margin-bottom: 6px;
          white-space: nowrap;
        }
        .control-panel .info-icon { line-height: 1; }
        /* Button groups come with a line break after their label; with block
           labels it would push them a line below the dropdowns. */
        .control-panel .shiny-input-container > br { display: none; }
        .control-panel .form-group { margin-bottom: 14px; width: 100%; }
        .control-panel .row:last-child .form-group { margin-bottom: 6px; }
        .control-panel .row > [class*='col-'] { padding-left: 12px; padding-right: 12px; }

        /* Dropdowns: full column width, same height as the button groups. */
        .control-panel .bootstrap-select { width: 100% !important; }
        .control-panel .bootstrap-select > .btn {
          height: 34px; padding: 6px 12px; font-size: 13px; line-height: 20px;
        }
        .control-panel .btn-group-justified .btn { height: 34px; line-height: 20px; }
        .control-panel .irs { margin-top: -6px; }

        .bootstrap-select .btn { border-radius: 6px; }
        /* A choice the selected index is not published for cannot be clicked. */
        .control-panel .btn.disabled { pointer-events: none; }
        .control-panel .btn-group-justified .btn {
          padding-left: 4px; padding-right: 4px;
          overflow: hidden; text-overflow: ellipsis;
        }

        .table-section {
          margin-top: 20px;
          padding-top: 16px;
          border-top: 1px solid #e3e8ee;
        }
        .section-title {
          font-size: 15px;
          font-weight: 700;
          color: #2c3e50;
          margin-bottom: 10px;
        }

        table.dataTable td, table.dataTable th { white-space: nowrap; }

        .info-icon {
          display: inline-block; position: relative;
          cursor: help; color: #7f8c8d; font-size: 12px; margin-left: 5px;
          vertical-align: middle;
        }
        .info-tip {
          visibility: hidden; opacity: 0;
          background: rgba(44,44,44,0.95); color: #fff;
          padding: 8px 12px; border-radius: 5px;
          position: absolute; z-index: 9999;
          top: 125%; left: 0;
          width: 290px; white-space: normal;
          font-size: 12px; font-weight: 400; line-height: 1.6;
          transition: opacity 0.15s; pointer-events: none;
          box-shadow: 0 2px 8px rgba(0,0,0,0.25);
        }
        .info-icon:hover .info-tip { visibility: visible; opacity: 1; }
        /* Right-hand controls: open the tip leftwards so it stays on screen. */
        .control-panel .row > div:last-child .info-tip { left: auto; right: 0; }

        .map-note {
          margin: 0 0 12px 0;
          padding: 10px 14px;
          background: #f8f9fa;
          border-left: 3px solid #95a5a6;
          border-radius: 4px;
          color: #555; font-size: 13px;
        }
        .map-note--warning { border-left-color: #f0ad4e; background: #fff8ec; }

        .cmp-title {
          font-size: 14px; font-weight: 700; color: #2c3e50; margin: 6px 0;
        }
        .cmp-title span { font-weight: 400; color: #6c757d; }

        .about { max-width: 860px; font-size: 15px; line-height: 1.6; }
        .about h4 { font-weight: 700; color: #2c3e50; margin-top: 22px; }

        /* Shown while the server is working (Shiny sets html.shiny-busy). */
        .loading-wrapper { position: relative; }
        /* Short enough that the table below shows without scrolling. */
        #map { min-height: 320px; }
        .loading-overlay {
          position: absolute;
          top: 50%; left: 50%;
          transform: translate(-50%, -50%);
          background: rgba(255,255,255,0.92);
          padding: 14px 22px;
          border-radius: 8px;
          font-size: 14px;
          font-weight: 500;
          color: #34495e;
          z-index: 1000;
          display: flex;
          align-items: center;
          gap: 10px;
          box-shadow: 0 2px 12px rgba(0,0,0,0.12);
          pointer-events: none;
          opacity: 0;
          transition: opacity 0.2s;
        }
        html.shiny-busy .loading-overlay { opacity: 1; transition-delay: 0.5s; }
      "))
    ),
    conditionalPanel(condition = "input.nav != 'tab_about'", control_panel)
  ),

  footer = div(
    id = "sticky-footer",
    tags$a(href = "https://med.stanford.edu/", target = "_blank",
           tags$img(src = "assets/stanf_logo.png", height = "32px",
                    style = "vertical-align: middle;")),
    div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$a(href = "https://med.stanford.edu/qsu.html", target = "_blank",
             "Quantitative Sciences Unit",
             style = "text-decoration:none; color:#555; font-weight:500; font-size:16px;"),
      tags$a(href = "https://med.stanford.edu/summerhanlab.html", target = "_blank",
             "– Han Lab",
             style = "text-decoration:none; color:#555; font-weight:500; font-size:16px;")
    )
  ),

  # ── Tab 1: Map ───────────────────────────────────────────────────────────────
  tabPanel(
    title = tagList(icon("map"), " Map"),
    value = "tab_map",

    fluidRow(column(12, uiOutput("map_note"))),

    fluidRow(column(12,
      div(class = "loading-wrapper",
        leafletOutput("map", height = "45vh"),
        loading_overlay
      )
    )),

    fluidRow(column(12,
      div(class = "table-section",
          div(class = "section-title", textOutput("table_title", inline = TRUE)),
          div(style = "margin-bottom: 8px;",
              downloadButton("download_csv", "Download CSV",
                             class = "btn-sm btn-default")),
          DTOutput("table")
      )
    ))
  ),

  # ── Tab 2: Raw vs. best ─────────────────────────────────────────────────────
  tabPanel(
    title = tagList(icon("chart-bar"), " Raw vs. Best"),
    value = "tab_compare",

    fluidRow(column(12,
      div(class = "map-note",
          tags$i(class = "fa fa-circle-info", style = "margin-right: 6px; color: #95a5a6;"),
          "The two versions of the selected index, side by side, for the same areas. ",
          "The Version control does not apply here.")
    )),

    div(class = "loading-wrapper",
      fluidRow(
        column(6, div(class = "cmp-title", uiOutput("cmp_title_raw")),
                  leafletOutput("cmp_map_raw", height = "65vh")),
        column(6, div(class = "cmp-title", uiOutput("cmp_title_best")),
                  leafletOutput("cmp_map_best", height = "65vh"))
      ),
      loading_overlay
    )
  ),

  # ── Tab 3: Index vs. index ──────────────────────────────────────────────────
  tabPanel(
    title = tagList(icon("right-left"), " Index vs. Index"),
    value = "tab_vs",

    fluidRow(column(12, uiOutput("vs_note"))),

    div(class = "loading-wrapper",
      fluidRow(
        column(6, div(class = "cmp-title", uiOutput("vs_title_a")),
                  leafletOutput("vs_map_a", height = "65vh")),
        column(6, div(class = "cmp-title", uiOutput("vs_title_b")),
                  leafletOutput("vs_map_b", height = "65vh"))
      ),
      loading_overlay
    )
  ),

  # ── Tab 4: About ─────────────────────────────────────────────────────────────
  tabPanel(
    title = tagList(icon("circle-info"), " About"),
    value = "tab_about",

    div(class = "about",
      p("This app maps neighborhood socioeconomic indices for US census tracts and block groups, ",
        "built from the US Census Bureau's American Community Survey (ACS) 5-year estimates. ",
        "Every value shown is read with the ",
        tags$a(href = "https://github.com/HanLabCollaboration/ReproducibleSDOH", target = "_blank",
               "ReproducibleSDOH"),
        " R package from the pre-computed files in ",
        tags$a(href = "https://github.com/HanLabCollaboration/ReproducibleSDOH-data", target = "_blank",
               "ReproducibleSDOH-data"), "."),
      h4("Indices"),
      tags$ul(lapply(INDICES, function(spec) {
        tags$li(tags$b(spec$label), paste0(" (", spec$higher_text, "). "), spec$about)
      })),
      h4("Things to know"),
      tags$ul(
        tags$li("Years name the last year of the ACS 5-year period: 2022 is the 2018–2022 ACS."),
        tags$li("Coverage is the 50 states and the District of Columbia."),
        tags$li("Area definitions change in 2020: years up to 2019 use 2010 census tracts and block groups, later years use the 2020 definitions."),
        tags$li("Population is the ACS total population (table B01003) for the same 5-year period."),
        tags$li("Areas in grey have no score. Areas with no population have no score in either version."),
        tags$li(sprintf("Boundaries for %d are built into the app; other years are downloaded the first time they are used. Whole-state maps use thinned boundaries to stay responsive.", STARTUP_YEAR))
      ),
      h4("Get the data in R"),
      tags$pre(
'remotes::install_github("HanLabCollaboration/ReproducibleSDOH")
library(ReproducibleSDOH)

yost <- getYostIndex(geo = "tract", year = 2024, states = "CA")'
      )
    )
  )
)
