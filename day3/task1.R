library(readr)
library(dplyr)

make_coords <- function(input, positive, negative) {
  input %>%
    replace(input == positive, 1) %>%
    replace(input == negative, -1) %>%
    replace((input != positive) & (input != negative), 0) %>%
    as.numeric %>%
    cumsum
}

input <- read_file("input.txt") %>%
  trimws %>%
  strsplit("") %>%
  unlist

coords <- data.frame(
  x = make_coords(input, ">", "<"),
  y = make_coords(input, "^", "v")
)

coords %>% distinct %>% nrow
