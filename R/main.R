
# Setup -------------------------------------------------------------------
## Options ----
options(readr.show_col_types = FALSE)

# Functions ---------------------------------------------------------------
telescope <- function() {
  appDir <- system.file("shiny", package = "telescope")
  runApp(appDir = appDir)
}