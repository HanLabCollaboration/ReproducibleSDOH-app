# ReproducibleSDOH-app

Shiny app that maps the neighborhood socioeconomic indices published by
[ReproducibleSDOH](https://github.com/HanLabCollaboration/ReproducibleSDOH):
the Yost Index, the Neighborhood Deprivation Index (Messer, Powell-Wiley) and
the Index of Concentration at the Extremes (ICE), for census tracts and block
groups, together with each area's population.

## Run it

```r
remotes::install_github("HanLabCollaboration/ReproducibleSDOH")
install.packages(c("shiny", "shinythemes", "shinyWidgets", "leaflet", "sf", "DT", "viridisLite"))
shiny::runApp()
```

## Files

```
app.R        Loads global.R, ui.R and server.R.
global.R     Index registry, data loading, map styling.
ui.R         Controls and the four tabs: Map, Raw vs. Best, Index vs. Index, About.
server.R     Reactive logic.
data/        2024 boundaries at full resolution, one file per state (about 70 MB).
data-raw/    Script that builds data/.
deploy.R     Publishes to shinyapps.io.
www/         Logo.
```

## Where the data comes from

The app stores **boundaries for 2024 only**, at full resolution, so the first map needs
no boundary download. Everything else is read at run time with the
`get*()` functions of ReproducibleSDOH, which download the published files and
cache them:

| What | Source |
|---|---|
| Index scores, area names, population | `getYostIndex()`, `getNDI()`, `getICE()` with `info = TRUE` |
| Boundaries, 2024 | `data/boundaries_2024/` |
| Boundaries, 2013–2023 | `getGeoInfo(geometry = TRUE)`: full resolution, 60–100 MB per geography and year on first use |
| Quintiles | computed in the app (see `to_quintile()` in `global.R`) |

Years start in 2013: the 2011 and 2012 tract boundaries are about 520 MB each.

To rebuild `data/`:

```bash
Rscript data-raw/build_startup_boundaries.R
```

## Adding an index

Add one entry to `INDICES` in `global.R`. The controls, data calls and map
styling all read from it.

## Settings

Copy `.Renviron.example` to `.Renviron` (git ignores it) and fill in:

- `CARTO_API_KEY`: optional. With a [CARTO basemaps key](https://carto.com/basemaps/apikey)
  the map uses CARTO's Positron tiles; without one it uses Esri's light gray basemap.
- `SHINYAPPS_NAME`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`: used by `deploy.R` only.

## Deploy

```bash
Rscript deploy.R
```
