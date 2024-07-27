library(tidyverse)
library(zeallot)
unpack <- wrapr::unpack

get_input <- function() {
  c(my_subs, my_expr) %<-% (
    read_file("input.txt") %>%
    str_split_fixed("\n\n", 2) %>%
    unlist
  )
  my_subs <- 
    my_subs %>%
    str_split("\n") %>%
    unlist %>%
    lapply(function(row) {
      row %>%
        str_split_fixed(" => ", 2) %>%
        unlist
    }) %>%
    rbind
  names(my_subs) <- c("original", "substitution")
  list(
    substitutions = my_subs,
    expression = my_expr
  )
}

replace_at <- function(string, from, to, new_str) {
  str_start <- string %>% substring(0, from - 1)
  str_end <- string %>% substring(to + 1)
  paste0(str_start, new_str, str_end)
}

get_derivatives <- function(expression, from, to) {
  matches <- str_locate_all(expression, from)[[1]]
  sapply(seq_len(nrow(matches)), function(index) {
    c(start, end) %<-% matches[index, ]
    expression %>% replace_at(start, end, to)
  })
}

unpack[subs = substitutions, expr = expression] <- get_input()
subs %>%
  sapply(function(sub) {
    c(from, to) %<-% sub
    get_derivatives(expr, from, to)
  }) %>%
  unlist %>%
  unique %>%
  length
