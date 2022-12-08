library(tidyverse)

input <- read_fwf("input.txt") |> 
  transmute(x = str_split(X1, "")) |> 
  unnest_wider(x, names_sep = "") |> 
  mutate(across(everything(), as.integer)) |> 
  as.matrix()

# Create df with one row per tree and variables for its containing row & col
expand_grid(
  col_pos = 1:nrow(input),
  row_pos = 1:ncol(input)
) |> 
  mutate(
    tree_idx = row_number(),
    row_list = map(col_pos, ~ unname(as.matrix(input)[.x, ])),
    col_list = map(row_pos, ~ unname(as.matrix(input)[, .x])),
  ) |> 
  
  # For each tree, construct its treeline looking outward in each direction
  pivot_longer(
    c(col_pos, row_pos, col_list, row_list),
    names_to = c("axis", ".value"),
    names_sep = "_"
  ) |> 
  mutate(
    split = map2(
      list,
      pos,
      ~ split(.x, c(rep("bwd", .y - 1), "curr_tree", rep("fwd", length(.x) - .y)))
    )
  ) |> 
  unnest_wider(split) |> 
  mutate(bwd = map(bwd, rev)) |> 
  pivot_longer(c(fwd, bwd), names_to = "dir", values_to = "treeline") |> 
  
  # Check if each is the tallest tree in each direction & count visible trees
  mutate(
    is_tallest = map2_lgl(curr_tree, treeline, ~ all(.x > .y)),
    num_visible = map2_int(curr_tree, treeline, function(curr_tree, treeline) {
      ifelse(
        every(treeline, ~.x < curr_tree), 
        length(treeline),
        detect_index(treeline, ~ .x >= curr_tree)
      )
    })
  ) |> 
  
  # Summarize visibility & scenic scores from all 4 directions for each tree
  group_by(tree_idx) |> 
  summarize(
    is_visible = any(is_tallest),
    scenic_score = prod(num_visible)
  ) |> 
  ungroup() |> 
  
  # Compute total trees visible from forest edge & max scenic score in forest
  summarize(
    total_visible = sum(is_visible),
    max_scenic_score = max(scenic_score)
  )

