
# Setup -------------------------------------------------------------------
## Options ----
options(telescope.reprocess_data = FALSE)
options(readr.show_col_types = FALSE)

# Functions ---------------------------------------------------------------
telescope <- function() {
  appDir <- system.file("shiny", package = "telescope")
  runApp(appDir = appDir)
}