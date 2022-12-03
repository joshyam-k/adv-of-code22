library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day3.txt")))

scores <- c(1:52)
names(scores) <- c(letters, LETTERS)

## part 1

input %>% 
  mutate(comp1 = str_sub(x, 1, str_length(x)/2),
         comp2 = str_sub(x, (str_length(x)/2 + 1), str_length(x))) %>% 
  mutate(comp1 = str_split(comp1, ""),
         comp2 = str_split(comp2, "")) %>% 
  rowwise() %>% 
  mutate(common = intersect(comp1, comp2)) %>% 
  ungroup() %>% 
  mutate(score = scores[common]) %>% 
  summarise(total = sum(score))


## part 2

input %>% 
  mutate(group = sort(rep(1:100, 3))) %>% 
  mutate(x = str_split(x, "")) %>% 
  group_by(group) %>% 
  summarise(common = reduce(x, intersect)) %>% 
  ungroup() %>% 
  mutate(score = scores[common]) %>% 
  summarise(total = sum(score))

  
