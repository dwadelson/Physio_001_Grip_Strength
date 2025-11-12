library(shiny)

# Set working directory to the app's location (only works inside RStudio)
if (interactive()) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)