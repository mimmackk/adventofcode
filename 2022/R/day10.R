library(tidyverse)

signal <- read_lines("day10/input.txt") |>
  enframe(name = "cmd_num") |>

  # Convert multi-cycle commands into list of changes to X at each cycle
  separate(value, into = c("cmd", "V"), sep = " ", convert = TRUE) |>
  transmute(cmd_num, v1 = 0, v2 = V) |>
  pivot_longer(
    c(v1, v2),
    names_to = "cycle_count",
    values_to = "x_change",
    values_drop_na = TRUE
  ) |>
  pull(x_change) |>

  # Iterate through changes to X to get value of X during each cycle
  accumulate(`+`, .init = 1) |>
  enframe(name = "cycle", value = "X") |>
  slice_head(n = -1)

# Part 1: Sum the signal strength at 20th cycle & every 40 cycles after that

signal |>
  filter(cycle %% 40 == 20) |>
  mutate(signal_strength = cycle * X) |>
  pull(signal_strength) |>
  sum()

# Part 2: Draw pixel when position of 3px-wide sprite overlaps w/ CRT position
signal |>
  mutate(
    row = floor((cycle - 1) / 40),
    col = (cycle - 1) %% 40
  ) |>
  mutate(pixel = if_else(X <= col + 1 & X >= col - 1, "#", ".")) |>
  group_by(row) |>
  summarize(val = str_c(pixel, collapse = "")) |>
  pull(val) |>
  cat(sep = "\n")
