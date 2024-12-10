library(tidyverse)

find_marker <- function(file, marker_length) {

  read_lines(file) |>

    # Convert input stream to vector of individual characters
    str_split("") |>
    unlist() |>
    enframe(name = "idx", value = "char") |>

    # Construct sequences of next n chars and count # of unique chars in each
    transmute(
      marker_idx = idx + marker_length - 1,
      char_seq = reduce(
        .x = map(0:(marker_length - 1), ~ lead(char, n = .x)),
        .f = str_c
      ),
      n_unique = map_int(
        char_seq,
        ~ .x |>
          str_split("") |>
          unlist() |>
          unique() |>
          length()
      )
    ) |>

    # Extract first instance where all n chars are unique
    filter(n_unique == marker_length) |>
    pull(marker_idx) |>
    min()
}

find_marker(file = "day06/input.txt", marker_length = 4)
find_marker(file = "day06/input.txt", marker_length = 14)
