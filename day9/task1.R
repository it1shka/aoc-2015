library(tidyverse)
library(foreach)
library(doParallel)
library(combinat)

registerDoParallel(cores = detectCores())

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

get_routes <- function(frame) {
  c(input$from, input$to) |>
    unique() |>
    permn() %>%
    do.call(rbind, .) |>
    as_tibble()
}

get_weighted_routes <- function(distances, routes) {
  route_dist <- foreach(i = seq_len(nrow(routes)), .combine = "c") %dopar% {
    paste0("Started #", i) |> print()
    total <- 0
    route <- routes[i, ]
    for (j in seq_len(length(route) - 1)) {
      a <- route[[j]]
      b <- route[[j + 1]]
      distance <- distances |>
        filter((from == a & to == b) | (from == b & to == a)) |>
        select(distance) |>
        unlist()
      total <- total + distance
    }
    paste0("Distance #", i, " is ", total) |> print()
    total
  }
  routes |>
    mutate(distance = route_dist)
}

input <- read_input()
routes <- get_routes(input)
weighted_routes <- get_weighted_routes(input, routes)
min_distance <-
  weighted_routes |>
  select(distance) |>
  min()
min_distance
