library(tidyverse)

house_presents <- function(house) {
  seq_len(house) %>%
    (\(elves) elves[house %% elves == 0] * 10) %>%
    sum
}

find_house <- function(limit) {
  house <- 1
  while (TRUE) {
    presents <- house_presents(house)
    if (presents >= limit) {
      return(house)
    }
    paste0("House #", house, ": ", presents) |> print()
    house <- house + 1
  }
}

limit <- 33100000
find_house(limit)
