library(tidyverse)
library(combinat)

read_input <- function() {
  rows <-
    read_file("input.txt") |>
    str_trim() |>
    str_split("\n") |>
    unlist()
  frame <-
    rows |>
    str_split("\\sto\\s|\\s=\\s") %>%
    do.call(rbind, .) |>
    as_tibble()
  colnames(frame) <- c("from", "to", "distance")
  frame |>
    mutate(distance = as.integer(distance))
}

input <- read_input()
cities <- c(input$from, input$to) |> unique()
routes <- permn(cities)
head(routes)
