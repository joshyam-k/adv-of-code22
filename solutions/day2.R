library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day2.txt"))) %>% 
  mutate(x = str_trim(x, "both")) %>% 
  separate(x, into = c("opp", "you"), sep = " ") 

## part 1

results <- matrix(
  c(3, 6, 0,
    0, 3, 6,
    6, 0, 3),
  byrow = T,
  ncol = 3,
  dimnames = list( c("A", "B", "C"), c("X", "Y", "Z"))
)

match <- c("X" = 1, "Y" = 2, "Z" = 3)

input %>% 
  rowwise() %>% 
  mutate(score = results[opp, you] + match[you]) %>% 
  ungroup() %>% 
  summarise(total = sum(score))
  
## part two

outcome <- matrix(
  c("Z", "X", "Y",
    "X", "Y", "Z",
    "Y", "Z", "X"),
  byrow = T,
  ncol = 3,
  dimnames = list(c("A", "B", "C"),c("X", "Y", "Z"))
)

input %>% 
  rowwise() %>% 
  mutate(your_move = outcome[opp, you],
         score = results[opp, your_move] + match[your_move]) %>% 
  ungroup() %>% 
  summarise(total = sum(score))


