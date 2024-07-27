library(tidyverse)
library(zeallot)

get_input_frame <- function() {
  lines <-
    read_file("input.txt") %>%
    str_trim %>%
    str_split("\n") %>%
    unlist
  parsed_lines <- lapply(lines, function(line) {
    c(name, params) %<-% str_split_fixed(line, ": ", 2) %>% unlist
    params <-
      params %>%
      str_split(", ") %>%
      unlist %>%
      str_split(": ") %>%
      unlist
    keys <- c("name", params[seq.int(1, length(params), 2)])
    values <- c(name, params[seq.int(2, length(params), 2)])
    tibble(!!!setNames(as.list(values), keys))
  })
  bind_rows(parsed_lines)
}

get_checks <- function(sue_data) {
  requirements <- tibble(
    children = 3,
    cats = 7,
    samoyeds = 2,
    pomeranians = 3,
    akitas = 0,
    vizslas = 0,
    goldfish = 5,
    trees = 3,
    cars = 2,
    perfumes = 1
  )
  result <- colnames(requirements) %>%
    lapply(function(req_name) {
      sue_vector <- sue_data[req_name] %>%
        unlist %>%
        as.integer
      req_value <- requirements[req_name] %>%
        unlist
      (sue_vector == req_value) | is.na(sue_vector)
    })
  do.call(cbind, result) %>%
    as_tibble
}

input <- get_input_frame()
checks <- get_checks(input)
for (i in seq_len(nrow(input))) {
  if (checks[i, ] %>% all) {
    print(input[i, "name"])
  }
}
