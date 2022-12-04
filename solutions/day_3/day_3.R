library(tidyverse)
library(adventdrob)
dat <- advent_input(day = 3, year = 2022) %>%
  rename(combined = x)


test_dat <- tibble(combined = c("vJrwpWtwJgWrhcsFMMfFFhFp",
"jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
"PmmdzqPrVvPwwTWBwg",
"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
"ttgJtRGJQctTZtZT",
"CrZsJsPPZsGzwwsLwLmpwMDw"))


get_priority <- function(x) which(x == c(letters, LETTERS))

generalized_intersection <- function(...) reduce(list(...), intersect)

dat %>%
  mutate(n_items = str_length(combined)) %>%
  transmute(first = str_sub(combined, end = n_items / 2),
            second = str_sub(combined, start = n_items / 2 + 1)) %>%
  mutate(across(everything(), ~str_split(., ""))) %>%
  mutate(common_element = map2_chr(first, second, intersect)) %>%
  mutate(priority = map_int(common_element, get_priority)) %>%
  pull(priority) %>%
  sum()


dat %>%
  group_by(group_num = ceiling(row_number() / 3)) %>%
  mutate(position_in_group = row_number()) %>%
  mutate(combined = str_split(combined, "")) %>%
  pivot_wider(names_from = position_in_group, values_from = combined) %>%
  mutate(common_element = pmap_chr(list(`1`, `2`, `3`), generalized_intersection)) %>%
  mutate(priority = map_int(common_element, get_priority)) %>%
  pull(priority) %>%
  sum()
