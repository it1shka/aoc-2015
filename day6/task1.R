library(tidyverse)
library(zeallot)

execute_instructions <- function(instructions) {
  bulbs <- matrix(FALSE, 1000, 1000)
  for (i in seq_len(nrow(instructions))) {
    c(kind, start, end) %<-% instructions[i, ]
    c(x1, y1) %<-% (str_split_1(start, ",") %>% as.numeric)
    c(x2, y2) %<-% (str_split_1(end, ",") %>% as.numeric)

    bulbs[(x1 + 1):(x2 + 1), (y1 + 1):(y2 + 1)] <-
      switch(kind,
        "turn on" = TRUE,
        "turn off" = FALSE,
        "toggle" = !bulbs[(x1 + 1):(x2 + 1), (y1 + 1):(y2 + 1)]
      )
  }
  sum(bulbs)
}

input <-
  read_file("input.txt") %>%
  str_trim

kind <-
  input %>%
  str_extract_all("toggle|turn on|turn off") %>%
  unlist

start <-
  input %>%
  str_extract_all("\\d+\\,\\d+ through") %>%
  sapply(function(row) row %>% str_extract("\\d+\\,\\d+"))

end <-
  input %>%
  str_extract_all("\\d+\\,\\d+\n|\\d+\\,\\d+$") %>%
  sapply(str_trim)

instructions <- tibble(kind, start, end)

head(instructions)

execute_instructions(instructions)
