library(tidyverse)

get_input <- function() {
  read_file("input.txt") |>
    str_split("\n") |>
    map(\(row) {
      row |>
        str_trim() |>
        str_split("") |>
        unlist() %>%
        replace(., . == "#", TRUE) %>%
        replace(., . == ".", FALSE)
    }) |>
    unlist() |>
    as.logical() |>
    matrix(nrow = 100, ncol = 100)
}

next_step <- function(field) {
  output <- matrix(nrow = nrow(field), ncol = ncol(field))
  for (i in field |> nrow() |> seq_len()) {
    for (j in field |> ncol() |> seq_len()) {
      is_on <- field[i, j]
      row_idx <- max(i - 1, 0):min(i + 1, nrow(field))
      col_idx <- max(j - 1, 0):min(j + 1, ncol(field))
      neighbors <- field[row_idx, col_idx] |>
        as.numeric() |>
        sum()
      neighbors <- neighbors - as.numeric(is_on)
      output[i, j] <- (is_on && (neighbors == 2 || neighbors == 3)) ||
        (!is_on && neighbors == 3)
    }
  }
  output
}

get_input() %>%
  reduce(1:100, \(acc, elem) next_step(acc), .init = .) |>
  as.numeric() |>
  sum()
