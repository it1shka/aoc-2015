library(readr)
library(dplyr)

get_coordinates_vector <- function(input, positive, negative) {
  input %>%
    replace(input == positive, 1) %>%
    replace(input == negative, -1) %>%
    replace((input != positive) & (input != negative), 0) %>%
    as.numeric %>%
    cumsum
}

get_coordinates_frame <- function(input_vector) {
  data.frame(
    x = get_coordinates_vector(input_vector, ">", "<"),
    y = get_coordinates_vector(input_vector, "^", "v")
  )
}

main <- function() {
  input <-
    read_file("input.txt") %>%
    trimws %>%
    strsplit("") %>%
    unlist

  first <-
    input[seq(1, length(input), 2)] %>%
    get_coordinates_frame

  second <-
    input[seq(2, length(input), 2)] %>%
    get_coordinates_frame

  first %>%
    rbind(second) %>%
    distinct %>%
    nrow
}

main()
