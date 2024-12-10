library(tidyverse)

count_max <- function(file, num_top_elves) {

  # Import & format snack list, numbered by elf
  read_lines(file) |>
    as_tibble() |>
    transmute(
      cal = as.integer(value),
      elf_id = cumsum(is.na(cal)) + 1
    ) |>
    filter(!is.na(cal)) |>

    # Compute sum for each elf
    group_by(elf_id) |>
    summarize(total_cal = sum(cal)) |>

    # Get the top n elves and combine their totals
    slice_max(total_cal, n = num_top_elves) |>
    pull(total_cal) |>
    sum()
}

path <- "day01/input.txt"

count_max(path, 1)
count_max(path, 3)
