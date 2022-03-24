## Write Common Data ====

## Data Quality Reference

library(dplyr)
library(devtools)

# you will need to modify this to the right path.
qref <- readr::read_csv(file = "./data-raw/resources/dq_ref.csv")
qref <- qref %>%
  mutate(ranges = case_when(
    is.na(range_min) | is.na(range_max) ~ NA_character_,
    TRUE ~ paste0("[", range_min, ", ", range_max, "]")
  ))

load("./data-raw/resources/possible_values.RData")

qref <- dplyr::left_join(qref, qref_pv, by = "code_name") %>%
  select(.data$code_name,
         .data$short_name,
         .data$long_name,
         .data$primary_column,
         .data$type,
         .data$class,
         .data$ranges,
         .data$possible_values,
         .data$periodicity,
         .data$prop_missing,
         .data$assumed_units,
         .data$dist_compare,
         .data$notes)

use_data(
  qref,
  internal = TRUE, overwrite = TRUE, compress = "xz"
)
