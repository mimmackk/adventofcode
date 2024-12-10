library(tidyverse)

path <- "day04/input.txt"

read_csv(path, col_names = FALSE) |>
  separate(X1, into = c("start1", "end1"), sep = "-", convert = TRUE) |>
  separate(X2, into = c("start2", "end2"), sep = "-", convert = TRUE) |>
  mutate(
    range1 = map2(start1, end1, ~ .x:.y),
    range2 = map2(start2, end2, ~ .x:.y),
    contained = map2_lgl(range1, range2, ~ all(.x %in% .y) | all(.y %in% .x)),
    overlap   = map2_lgl(range1, range2, ~ length(intersect(.x, .y)) > 0)
  ) |>
  summarize(num_contained = sum(contained), num_overlap = sum(overlap))
