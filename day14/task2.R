library(tidyverse)

get_input_frame <- function() {
  lines <-
    read_file("input.txt") %>%
    str_trim %>%
    str_split("\n") %>%
    unlist
  parsed_lines <- lapply(lines, function(line) {
    parts <- line %>% str_split(" ") %>% unlist
    indexes <- c(4, 7, 14)
    parts[indexes] %>% as.numeric
  })
  input_frame <-
    do.call(rbind, parsed_lines) %>%
    as_tibble
  colnames(input_frame) <- c("speed", "fly", "rest")
  input_frame
}

perform_competition <- function(raindeers, seconds) {
  for (second in 0:(seconds - 1)) {
    flying <- second %% (raindeers$fly + raindeers$rest) < raindeers$fly
    delta <- raindeers$speed * ifelse(flying, 1, 0)
    raindeers <- raindeers %>% mutate(
      distance = distance + delta,
      points = points + ifelse(distance == max(distance), 1, 0)
    )
  }
  raindeers
}

get_input_frame() %>%
  mutate(distance = 0, points = 0) %>%
  perform_competition(seconds = 2503) %>%
  select(points) %>%
  max
