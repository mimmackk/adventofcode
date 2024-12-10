library(tidyverse)

# Compare nested lists ---------------------------------------------------------

compare_nested <- function(a, b) {

  # Compare if both inputs are numeric
  if (is.numeric(a) & is.numeric(b)) {
    if (a < b)
      return(-1)
    if (a > b)
      return(1)
    if (a == b)
      return(0)
  }

  # Compare if only one input is numeric
  if (is.numeric(a) != is.numeric(b)) {
    if (is.numeric(a))
      return(compare_nested(list(a), b))
    if (is.numeric(b))
      return(compare_nested(a, list(b)))
  }

  # Compare if both inputs are lists
  i <- 1
  while (i <= min(length(a), length(b))) {
    result <- compare_nested(a[[i]], b[[i]])
    if (result %in% c(1, -1))
      return(result)
    i <- i + 1
  }
  # When all comparable values are equal, compare lengths
  return(compare_nested(length(a), length(b)))

}

# Determine pairwise order of nested lists -------------------------------------

sort_nested <- function(lst) {
  n <- length(lst)
  indices <- 1:n

  if (n == 0) return()
  if (n == 1) return(indices)

  # Bubble sort: loop through list and swap elements until sorted
  repeat {
    swap_occurred <- FALSE
    for (i in 1:(n - 1)) {
      j1 <- which(indices == i)
      j2 <- which(indices == i + 1)
      if (compare_nested(lst[[j1]], lst[[j2]]) == 1) {
        indices[j1] <- i + 1
        indices[j2] <- i
        swap_occurred <- TRUE
      }
    }
    if (!swap_occurred) break
  }
  return(indices)
}

# Read input -------------------------------------------------------------------

path <- "day13/input.txt"

# Read into a data frame and convert to indexed nested lists
df <- path |>
  read_lines() |>
  trimws() |>
  discard(~ .x == "") |>
  as_tibble_col(column_name = "txt") |>
  mutate(
    row_id   = row_number(),
    group_id = floor((row_id - 1) / 2) + 1,
    item_id  = (row_id - 1) %% 2 + 1,
    lst      = map(txt, jsonlite::fromJSON, simplifyVector = FALSE)
  )

# Part 1: Sum the indices of packet pairs that are in order --------------------

df |>
  select(group_id, item_id, lst) |>
  pivot_wider(
    names_from = item_id,
    names_prefix = "item_",
    values_from = lst
  ) |>
  mutate(
    comparison = map2_int(item_1, item_2, compare_nested)
  ) |>
  filter(comparison == -1) |>
  summarize(sum = sum(group_id))

# Part 2: Add new flagged packets, sort all, and multiply flagged indices ------

new_packets <- list("[[2]]", "[[6]]") |>
  map(jsonlite::fromJSON, simplifyVector = FALSE) |>
  as_tibble_col(column_name = "lst")

df |>
  transmute(lst, flag = FALSE) |>
  add_row(new_packets, flag = TRUE) |>
  mutate(ord = sort_nested(lst)) |>
  filter(flag) |>
  summarize(prod = prod(ord))
