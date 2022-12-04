library(tidyverse)
library(adventdrob)
# A for Rock, B for Paper, and C for Scissors
# X for Rock, Y for Paper, and Z for Scissors
dat <- advent_input(day = 2, year = 2022)

test_dat <- tribble(
     ~x,
  "A Y",
  "B X",
  "C Z"
  )


# Part 1 ------------------------------------------------------------------

# Your total score is the sum of your scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

# Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the score you would get if you were to follow the strategy guide.

# What would your total score be if everything goes exactly according to your strategy guide?
rps <- c("rock", "paper", "scissors")
ltw <- c("loss", "tie", "win")

game_key <-
  expand_grid(them = 1:3, me = 1:3) %>%
  mutate(outcome = (me - them + 1) %% 3) %>%
  mutate(score = me + 3 * outcome) %>%
  mutate(across(c(them, me), ~rps[.]),
         outcome = ltw[outcome + 1])


part_1_score <- function(dat) {
  dat %>%
    separate(col = x, into = c("them", "me")) %>%
    mutate(across(everything(), ~c("rock", "paper", "scissors")[as.numeric(factor(.))])) %>%
    left_join(game_key) %>%
    pull(score) %>%
    sum()
}

part_1_score(test_dat)
part_1_score(dat)


# Part 2 ------------------------------------------------------------------
# the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.

# what would your total score be if everything goes exactly according to your strategy guide?
part_2_score <- function(dat) {
  dat %>%
    separate(col = x, into = c("them", "outcome")) %>%
    mutate(them = rps[factor(them)],
           outcome = ltw[factor(outcome)]) %>%
    left_join(game_key) %>%
    pull(score) %>%
    sum()
}

part_2_score(test_dat)
part_2_score(dat)
