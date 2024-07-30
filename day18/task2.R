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

get_input()

# TODO: finish later
