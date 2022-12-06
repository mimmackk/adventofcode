library(tidyverse)

move_crates <- function(file, crate_mover_type = c(9000, 9001)) {
  
  # Read & format raw input ----------------------------------------------------
  
  input_raw <- read_lines(file)
  
  moves <- input_raw |>
    tail_while(~ .x != "") |> 
    str_extract_all("\\d+") |> 
    map(as.integer)
  
  stacks <- read_fwf(
    file,
    n_max = length(input_raw) - length(moves) - 2,
    col_types = "c"
  ) |>
    mutate(across(everything(), ~ str_extract(.x, "[A-Z]"))) |> 
    as.list() |> 
    map(discard, is.na) |> 
    map(rev)
  
  # Execute moves --------------------------------------------------------------
  
  for (curr_move in moves) {
    count <- curr_move[1]
    from  <- curr_move[2]
    to    <- curr_move[3]
    
    crates <- case_when(
      crate_mover_type == 9000 ~ rev(tail(stacks[[from]], count)),
      crate_mover_type == 9001 ~     tail(stacks[[from]], count)
    )
    
    stacks[[to]]   <- append(stacks[[to]], crates)
    stacks[[from]] <- head(stacks[[from]], -1 * count)
  }
  
  # Examine final top row of crates --------------------------------------------
  
  stacks |> 
    map(~ tail(.x, 1)) |> 
    str_c(collapse = "")
}

move_crates(file = "input.txt", crate_mover_type = 9000)
