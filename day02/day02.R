library(tidyverse)

score_game <- function(file, strategy_meaning = c("shape", "outcome")) {
  
  read_fwf(file, col_types = "c") |> 
    transmute(
      
      # Format shapes/strategies as numbers 1-3 for modular arithmetic
      opponent = as.numeric(factor(X1, levels = c("A", "B", "C"))),
      strategy = as.numeric(factor(X2, levels = c("X", "Y", "Z"))),
      
      # Determine your move, round outcome, and score based on strategy guide
      self = case_when(
        strategy_meaning == "shape"   ~ strategy,
        strategy_meaning == "outcome" ~ (opponent + strategy) %% 3 + 1
      ),
      outcome = case_when(
        strategy_meaning == "shape"   ~ (self - opponent + 1) %% 3 * 3,
        strategy_meaning == "outcome" ~ (strategy - 1) * 3
      ),
      score = self + outcome
      
    ) |> 
    pull(score) |> 
    sum()
}

score_game("input.txt", strategy_meaning = "shape")
score_game("input.txt", strategy_meaning = "outcome")