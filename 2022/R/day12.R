library(tidyverse)
library(igraph)

input_to_df <- function(input) {

  raw <- read_lines(input) |>
    trimws() |>
    discard(~ .x == "")

  # Convert input to a data frame of coordinates and elevations.
  raw |>
    str_split("") |>
    unlist() |>
    as_tibble() |>
    transmute(
      id = row_number(),
      letter = value,
      elevation = case_when(
        letter == "S" ~  Inf,
        letter == "E" ~ -Inf,
        .default = match(letter, letters)
      ),
      row = floor((id - 1) / str_length(raw[1]) + 1),
      col = (id - 1) %% str_length(raw[1]) + 1
    )

}

df_to_graph <- function(df) {

  # Flag whether each neighbor of each vertex is walkable
  neighbors <- df |>
    mutate(up   = lag(id), down  = lead(id), .by = col) |>
    mutate(left = lag(id), right = lead(id), .by = row) |>
    mutate(
      across(
        c(up, down, left, right),
        ~ elevation[.x],
        .names = "{.col}_elev"
      ),
      across(
        ends_with("_elev"),
        ~ (.x - elevation) <= 1,
        .names = "{str_remove(.col, '_elev')}_walkable"
      )
    ) |>
    rename_with(.cols = c(up, down, left, right), ~ str_c(.x, "_idx")) |>
    select(source_idx = id, ends_with(c("idx", "walkable")))

  # Construct a list of edges
  edge_list <- neighbors |>
    pivot_longer(
      !source_idx,
      names_to = c("target_dir", ".value"),
      names_sep = "_"
    ) |>
    rename(
      target_idx = idx,
      target_walkable = walkable
    ) |>
    filter(target_walkable == TRUE) |>
    pmap(function(source_idx, target_idx, ...) { c(source_idx, target_idx) }) |>
    unlist()

  # Convert to a directed graph
  g <- make_empty_graph() |>
    add_vertices(length(df$id)) |>
    add_edges(edge_list)

}

shortest_path_length <- function(g, source_idx, target_idx) {
  shortest_paths(g, from = source_idx, to = target_idx)$vpath[[1]] |>
    length() - 1
}

df <- input_to_df("day12/input.txt")
g <- df_to_graph(df)

# Get the indices of the start and end vertices
idx_start <- match("S", df$letter)
idx_end   <- match("E", df$letter)

# Compute shortest path from start to end
shortest_path_length(g, idx_start, idx_end)

# Loop over all starting locations and find the shortest path to the end
min_dist = Inf
for (i in c(idx_start, which(df$letter == "a"))) {
  cur <- shortest_path_length(g, i, idx_end)
  if (cur >= 0 & cur < min_dist) {
    min_dist <- cur
  }
}
min_dist

