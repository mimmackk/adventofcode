library(tidyverse)

dir_sizes <- read_lines("input.txt") |> 
  enframe(name = NULL) |> 
  mutate(
    
    # Determine path of each file by accumulating preceding cd terms
    path = value |> 
      str_extract("(?<=^\\$ cd ).*") |> 
      str_c("/") |> 
      replace_na("") |> 
      accumulate(
        ~ if (.y == "../") {
          str_remove(.x, "(?<=/)[a-z]+/$")
        } else {
          str_c(.x, .y)
        }
      ) |> 
      str_remove_all("^/|/$"),
    
    # Convert paths to lists of all containing directories
    dirs = path |>
      str_split("/") |> 
      map(~accumulate(.x, str_c, sep = "/"))
  ) |> 
  
  # Remove commands & directories from output and format file info as cols
  filter(!str_detect(value, "^\\$|dir ")) |>
  separate(value, into = c("size", "file"), sep = " ") |> 
  mutate(size = as.integer(size)) |> 
  
  # Convert nested lists of directories to long-format
  unnest_wider(dirs, names_sep = "_") |> 
  mutate(dirs_1 = "/") |> 
  pivot_longer(
    cols = matches("dirs_\\d+"), 
    names_to = NULL, 
    values_to = "dir",
    values_drop_na = TRUE
  ) |> 
  
  # Compute size of each directory
  group_by(dir) |> 
  summarize(size = sum(size)) |> 
  ungroup() 

# Part 1: Sum sizes of all directories with maximum size 100000  
dir_sizes |> 
  filter(size <= 100000) |> 
  pull(size) |> 
  sum()

# Part 2: Find size of the smallest necessary directory to delete
curr_system_size <- dir_sizes |> 
  filter(dir == "/") |> 
  pull(size)

dir_sizes |> 
  filter(size >= (30000000 - (70000000 - curr_system_size))) |> 
  slice_min(size) |> 
  pull(size)