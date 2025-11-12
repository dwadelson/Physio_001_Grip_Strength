library(tidyverse)

# Load the data
input_file <- "data/Class data for Muscle Fatigue Lab - Fall 2025 Section 11584.csv"
raw <- read_csv(input_file, skip = 1)

# Clean column names manually
colnames(raw)[1:2] <- c("id", "gender")

# Assign hand + condition labels
dominant_vars <- c(
  "before_exercise", "after_isotonic", "fatigue_time_isotonic",
  "strength_drop_post_isotonic_fatigue", "before_isometric_exercise",
  "after_isometric_exercise", "fatigue_time_isometric",
  "strength_drop_post_isometric_fatigue"
)

nondominant_vars <- dominant_vars

# Assign meaningful column names
colnames(raw)[3:10] <- paste("dominant", dominant_vars, sep = "_")
colnames(raw)[12:19] <- paste("non_dominant", nondominant_vars, sep = "_")

# Drop empty column and gather
tidy <- raw %>%
  select(id, gender, starts_with("dominant_"), starts_with("non_dominant_")) %>%
  pivot_longer(
    cols = -c(id, gender),
    names_to = c("hand", "condition"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    hand = recode(hand, "dominant" = "Dominant", "non" = "Non-dominant")
  ) %>%
  filter(!is.na(value))

# View the tidy data

print(tidy)

output_file <- file.path("data", paste0("tidy_", basename(input_file)))
write_csv(tidy, output_file)