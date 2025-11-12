# --- data_loader.R ---
  # Reads a CSV file from the /data directory
  data_loader <- function(filename) {
    if (is.null(filename) || filename == "") return(NULL)
    filepath <- file.path("data", filename)
    if (!file.exists(filepath)) return(NULL)
    readr::read_csv(filepath)
  }