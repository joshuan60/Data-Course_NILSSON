library(tidyverse)
library(readxl)
library(janitor)

# Reading in our files
gs_df  <- read_csv("insectgs.csv", locale = locale(encoding = "latin1"))
tip_df <- read_xlsx("List_of_4854_insects&10_outgroups.xlsx") |> clean_names()

# Resolving duplicate gs data and formatting the data frame to be able to make a
# direct comparison. I used the built in pipe rather than the tidyverse pipe for
# efficiency. 
gs_resolved <- gs_df |>
  mutate(method_rank = case_when(
    Method == "FCM" ~ 1,
    Method == "FIA" ~ 2,
    Method == "FD"  ~ 3,
    Method == "BCA" ~ 4,
    TRUE            ~ 5
  )) |>
  group_by(Name) |>
  slice_min(method_rank, with_ties = TRUE) |>
  summarise(
    gs_value      = mean(Mbp, na.rm = TRUE),
    gs_sd         = sd(Mbp, na.rm = TRUE),
    gs_cv         = gs_sd / gs_value,
    best_method   = first(Method),
    metamorphosis = first(Metamorphosis),
    n_records     = n(),
    .groups       = "drop"
  ) |>
  mutate(
    name_normalized = Name |>
      str_to_lower() |>
      str_replace_all("-", "") |>    # strip hyphens to match tip formatting
      str_squish()
  )

# Normalizing our tip names for matching 
# Use words 2 and 3 (genus + species) instead of last two words
# This correctly handles subspecies like Anthonomus_grandis_grandis
tip_df <- tip_df |>
  mutate(
    name_normalized = tip_names_in_data_matrix |>
      str_split("_") |>
      map_chr(~ paste(.x[2], .x[3], sep = " ")) |>
      str_to_lower() |>
      str_squish()
  )

# Comparing the twos sheets to find matches
gs_to_join <- gs_resolved |>
  select(name_normalized, gs_value, gs_sd, gs_cv,
         best_method, metamorphosis, n_records)

matches <- tip_df |>
  inner_join(gs_to_join, by = "name_normalized") |>
  select(
    tip_names_in_data_matrix,
    kingdom, phylum, subphylum, class, subclass, infraclass,
    cohort, order, suborder, infraorder,
    superfamily, family, subfamily, genus, species,
    metamorphosis, gs_value, gs_sd, gs_cv, best_method, n_records
  )

unmatched_tips <- anti_join(tip_df, gs_resolved, by = "name_normalized")
unmatched_gs   <- anti_join(gs_resolved, tip_df, by = "name_normalized")

# Printing summary information about our comparison
cat("Tree tips with GS data:   ", nrow(matches), "\n")
cat("Tree tips WITHOUT GS data:", nrow(unmatched_tips), "\n")
cat("GS species not in tree:   ", nrow(unmatched_gs), "\n")

# This code below allows us to look at the matches with high varience in the gs. 
high_variance <- matches |>
  filter(!is.na(gs_cv), gs_cv > 0.1) |>
  select(tip_names_in_data_matrix, species, gs_value, gs_sd,
         gs_cv, best_method, n_records) |>
  arrange(desc(gs_cv))
# Saving our matched_tips
write.csv(matches, file = "Matched_Tips.csv")

