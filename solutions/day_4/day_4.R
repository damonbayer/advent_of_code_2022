library(tidyverse)
library(adventdrob)
dat <- advent_input(day = 4, year = 2022) %>%
  rename(section_pairs = x)

processed_dat <-
  dat %>%
  separate(section_pairs, into = c("first", "second"), sep = ",") %>%
  mutate(pair_num = row_number()) %>%
  pivot_longer(-pair_num) %>%
  separate(value, c("min", "max")) %>%
  mutate(range = map2(min, max, ~.x:.y)) %>%
  select(-c(min, max)) %>%
  pivot_wider(names_from = name, values_from =  range) %>%
  mutate(any_subset = map2_lgl(first, second, ~{all(.x %in% .y) || all(.y %in% .x)})) %>%
  mutate(any_intersect = map2_lgl(first, second, ~length(intersect(.x, .y)) > 0))

sum(processed_dat$any_subset)

sum(processed_dat$any_intersect)
