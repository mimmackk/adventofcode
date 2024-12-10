compute_monkey_business <- function(input, num_rounds, part = c(1, 2)) {

  # Read in input --------------------------------------------------------------

  input <- read_lines(input) |>
    trimws() |>
    discard(~ .x == "")

  items <- str_match(input, "Starting items:(.*)")[,2] |>
    discard(is.na) |>
    str_split(",") |>
    map(parse_number)

  operations <- input |>
    keep(~ str_detect(.x, "Operation:")) |>
    str_replace("Operation: new = ", "~ ") |>
    str_replace_all("old", ".x") |>
    map(~ rlang::as_function(as.formula(.x)))

  div  <- parse_number(keep(input, ~ str_detect(.x, "Test:")))
  divt <- parse_number(keep(input, ~ str_detect(.x, "If true:")))
  divf <- parse_number(keep(input, ~ str_detect(.x, "If false:")))

  test <- pmap(
    list(div, divt, divf),
    ~ function(x) if_else(x %% ..1 == 0, ..2 + 1, ..3 + 1)
  )

  num_monkeys <- length(input) / 6
  activity    <- rep(0, num_monkeys)


  # Perform the tosses ---------------------------------------------------------

  lcm <- DescTools::LCM(div)
  worry_func <- if (part == 1) {
    function(x) floor(x / 3)
  } else if (part == 2) {
    function(x) x %% lcm
  }

  for (round in 1:num_rounds) {
    for (monkey in 1:num_monkeys) {
      for (item in items[[monkey]]) {
        worry <- worry_func(operations[[monkey]](item))
        toss  <- test[[monkey]](worry)
        items[[toss]] <- c(items[[toss]], worry)
      }
      activity[[monkey]] <- activity[[monkey]] + length(items[[monkey]])
      items[[monkey]] <- numeric(0)
    }
  }

  # Compute monkey business score ----------------------------------------------
  activity |>
    sort() |>
    tail(2) |>
    reduce(`*`)
}

compute_monkey_business("day11/input.txt", num_rounds = 20,    part = 1)
compute_monkey_business("day11/input.txt", num_rounds = 10000, part = 2)
