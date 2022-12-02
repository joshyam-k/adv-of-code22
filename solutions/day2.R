library(tidyverse)
library(here)

# opp: A for Rock, B for Paper, and C for Scissors
# you: X for Rock (1), Y for Paper (2), and Z for Scissors (3)

# our total score is the sum of your scores for each round. 
# The score for a single round is the score for the shape you selected
# (1 for Rock, 2 for Paper, and 3 for Scissors) plus 
# the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

# paper beats rock (Y beats A), rock beats scissors (X beats C),

# based off of opponents move
# if equal then draw
# A and we are Y then we win
# B and we are Z then we win
# C and we are X then we win
# otherwise we lose

input <- tibble(x = readLines(here("inputs", "day2.txt"))) %>% 
  mutate(x = str_trim(x, "both")) %>% 
  separate(x, into = c("opp", "you"), sep = " ") 

input %>% 
  mutate(score1 = case_when(
    you == "X" ~ 1,
    you == "Y" ~ 2,
    you == "Z" ~ 3
  )) %>% 
  mutate(
    score2 = case_when(
      opp == "A" & you == "X" ~ 3,
      opp == "B" & you == "Y" ~ 3,
      opp == "C" & you == "Z" ~ 3,
      opp == "A" & you == "Y" ~ 6,
      opp == "B" & you == "Z" ~ 6,
      opp == "C" & you == "X" ~ 6,
      T ~ 0
    )
  ) %>% 
  mutate(total = score1 + score2) %>% 
  summarise(pts = sum(total))


## part two

draw <- function(opp_input) {
  if(opp_input == "A") {
    return("X")
  } else if(opp_input == "B") {
    return("Y")
  } else {
    return("Z")
  }
}

lose <- function(opp_input) {
  if(opp_input == "A") {
    return("Z")
  } else if(opp_input == "B"){
    return("X")
  } else {
    return("Y")
  }
}

win <- function(opp_input) {
  if(opp_input == "A") {
    return("Y")
  } else if(opp_input == "B"){
    return("Z")
  } else {
    return("X")
  }
}



# X means you need to lose
# Y means you need to end the round in a draw,
# Z means you need to win

input %>% 
  rowwise() %>% 
  # mutate(
  #   draw = draw(opp),
  #   win = win(opp),
  #   lose = lose(opp)
  # )
  mutate(your_move = case_when(
      # draw
      you == "Y" ~ draw(opp),
      # loss
      you == "X" ~ lose(opp),
      # win
      you == "Z" ~ win(opp)
    )
  ) %>% 
  ungroup() %>% 
  mutate(score1 = case_when(
    your_move == "X" ~ 1,
    your_move == "Y" ~ 2,
    your_move == "Z" ~ 3
  )) %>% 
  mutate(score2 = case_when(
    you == "Y" ~ 3,
    you == "X" ~ 0,
    you == "Z" ~ 6
  )) %>% 
  mutate(score = score1 + score2) %>% 
  summarise(total = sum(score))
  

