library(tidyverse)

# STEP 1: Load the file
file_path <- "data/Revised Reorganized Class data for Muscle Fatigue Lab - Fall 2025 Section 11584.csv"
raw_all <- read_csv(file_path, col_names = FALSE, na = c("", "NA"))

# STEP 2: Identify the column labeled 'variable' and its index
variable_col_index <- which(apply(raw_all, 2, function(col) any(col == "variable", na.rm = TRUE)))
if (length(variable_col_index) != 1) stop("❌ ERROR: Could not uniquely identify the 'variable' column.")

# STEP 3: Detect header rows (non-empty entries in 'variable' column)
header_rows <- which(!is.na(raw_all[[variable_col_index]]) & raw_all[[variable_col_index]] != "")
if (length(header_rows) < 1) stop("❌ ERROR: No header rows detected in the 'variable' column.")

# STEP 4: Extract true categorical variable names (excluding id/gender)
all_header_vars <- raw_all[header_rows, variable_col_index, drop = TRUE] |> as.character()
category_names <- all_header_vars[!tolower(all_header_vars) %in% c("id", "gender")]
num_cats <- length(category_names)

# STEP 5: Use only the matching rows from the headers
header_values <- raw_all[header_rows[!tolower(all_header_vars) %in% c("id", "gender")], -variable_col_index]
combined_names <- apply(header_values, 2, function(x) paste(x, collapse = "*"))

# STEP 6: Build combined names for data columns (id, gender, then combined names)
combined_names <- apply(header_values, 2, paste, collapse = "*")
expected_colnames <- c("id", "gender", combined_names)

# STEP 7: Extract actual data rows and remove the 'variable' column
data_start_row <- max(header_rows) + 1
raw_data <- raw_all[data_start_row:nrow(raw_all), , drop = FALSE]
raw_data <- raw_data[, -variable_col_index]

# STEP 8: Check column count
if (ncol(raw_data) != length(expected_colnames)) {
  stop(paste("❌ ERROR: Column mismatch — data has", ncol(raw_data),
             "columns but expected", length(expected_colnames)))
}

# STEP 9: Assign column names and convert id/gender types
colnames(raw_data) <- expected_colnames
raw_data <- raw_data |>
  mutate(id = as.integer(id), gender = as.character(gender))

# STEP 10: Pivot longer and separate combined names
tidy <- raw_data |>
  pivot_longer(
    cols = -c(id, gender),
    names_to = "meta",
    values_to = "value"
  ) |>
  separate(meta, into = category_names, sep = "\\*", remove = TRUE) |>
  select(id, gender, all_of(category_names), value) |>
  filter(!is.na(value))

# STEP 11: Preview or write to file
glimpse(tidy)
# write_csv(tidy, "tidy_output.csv")