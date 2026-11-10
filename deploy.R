# deploy.R — publish the ReproducibleSDOH app to shinyapps.io
#
# Credentials come from .Renviron (see .Renviron.example), never from this file.
# ReproducibleSDOH must be installed from GitHub, so that shinyapps.io can
# install the same version:
#   remotes::install_github("HanLabCollaboration/ReproducibleSDOH")

readRenviron(".Renviron")
stopifnot(
  "Set SHINYAPPS_TOKEN and SHINYAPPS_SECRET in .Renviron" =
    nzchar(Sys.getenv("SHINYAPPS_TOKEN")) && nzchar(Sys.getenv("SHINYAPPS_SECRET")),
  "Install ReproducibleSDOH with remotes::install_github(), not from a local folder" =
    !is.null(utils::packageDescription("ReproducibleSDOH")$RemoteSha)
)

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_NAME", "qsushiny"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# The basemap key travels as its own file, without the deploy credentials.
key <- Sys.getenv("CARTO_API_KEY")
deploy_env <- file.path(tempdir(), ".Renviron")
writeLines(if (nzchar(key)) paste0("CARTO_API_KEY=", key) else character(0), deploy_env)

app_dir <- file.path(tempdir(), "ReproducibleSDOH-app")
unlink(app_dir, recursive = TRUE)
dir.create(app_dir)
file.copy(c("app.R", "global.R", "ui.R", "server.R", "data", "www"), app_dir, recursive = TRUE)
file.copy(deploy_env, app_dir)

rsconnect::deployApp(
  appDir         = app_dir,
  appPrimaryDoc  = "app.R",
  appName        = "reproducible-sdoh-app",
  account        = Sys.getenv("SHINYAPPS_NAME", "qsushiny"),
  launch.browser = TRUE,
  forceUpdate    = TRUE
)
