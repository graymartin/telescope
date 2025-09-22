

# Setup -------------------------------------------------------------------
## Options ----
options(telescope.default_dataframe = telescope::leep)

# Functions ---------------------------------------------------------------

#' Run telescope Shiny app
#'
#' @returns
#' @export
telescope <- function() {
  appDir <- system.file("shiny", package = "telescope")
  runApp(appDir = appDir)
}
