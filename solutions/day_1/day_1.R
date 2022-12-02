library(tidyverse)
library(adventdrob)

dat <-
  advent_input(day = 1, year = 2022) %>%
  mutate(x = as.integer(x)) %>%
  rename(calories = x)

# Part 1
# Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
total_calories_per_elf <-
  dat %>%
  mutate(elf_num = cumsum(is.na(calories))) %>%
  drop_na() %>%
  group_by(elf_num) %>%
  summarize(total_calories = sum(calories))

max(total_calories_per_elf$total_calories)

# Part 2
#the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories.

total_calories_per_elf %>%
  slice_max(order_by = total_calories,
            n = 3) %>%
  pull(total_calories) %>%
  sum()
