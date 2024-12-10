library(ctmle, include.only = "bound")
library(tidyverse)

unique_tail_spots <- function(input, num_knots) {

  # Convert head movements to sequence of locations on complex plane
  head_path <- read_lines(input) |>
    str_split(" ") |>
    map(~ rep(.x[[1]], .x[[2]])) |>
    reduce(c) |>
    recode("R" = 1 + 0i, "L" = -1 + 0i, "U" = 0 + 1i, "D" = 0 - 1i) |>
    accumulate(.init = 0 + 0i, .f = sum)

  # Find path of next knot given path of previous knot.
  move_next_knot <- function(prev_knot_path) {
    accumulate(
      .x = prev_knot_path,
      .f = function(tail = .x, head = .y) {
        diff <- head - tail
        if_else(
          max(abs(Re(diff)), abs(Im(diff))) <= 1,
          tail,
          tail + bound(Re(diff), c(-1, 1)) + bound(Im(diff), c(-1, 1)) * 1i
        )
      }
    )
  }

  # Iteratively compute path of each knot from head & to tail
  move_tail <- reduce(map(1:(num_knots - 1), ~ move_next_knot), compose)

  # Find number of unique locations in the tail's path
  length(unique(move_tail(head_path)))
}

unique_tail_spots("day09/input.txt", num_knots = 2)
unique_tail_spots("day09/input.txt", num_knots = 10)
