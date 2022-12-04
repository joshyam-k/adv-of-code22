library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day4.txt"))) %>% 
  separate(x, into = c("pair1", "pair2"), sep = ",") %>% 
  separate(pair1, into = c("pair1_1", "pair1_2"), sep = "-") %>% 
  separate(pair2, into = c("pair2_1", "pair2_2"), sep = "-") %>% 
  mutate_if(is.character,as.numeric) 
  
input %>% 
  rowwise() %>% 
  filter((between(pair1_1, pair2_1, pair2_2) & between(pair1_2, pair2_1, pair2_2)) |
           (between(pair2_1, pair1_1, pair1_2) & between(pair2_2, pair1_1, pair1_2))) %>% 
  nrow()


input %>% 
  rowwise() %>% 
  filter((between(pair1_1, pair2_1, pair2_2) | between(pair1_2, pair2_1, pair2_2)) |
           (between(pair2_1, pair1_1, pair1_2) | between(pair2_2, pair1_1, pair1_2))) %>% 
  nrow()
  
