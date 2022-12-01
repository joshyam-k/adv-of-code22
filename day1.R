library(tidyverse)
library(here)

input <- tibble(x = readLines(here("day1.txt")))

input %>% 
  mutate(flag = ifelse(x == "", row_number(), NA)) %>% 
  fill(flag, .direction = "up") %>% 
  group_by(flag) %>% 
  group_split()


