library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day1.txt")))

input_list <- input %>% 
  mutate(flag = ifelse(x == "", row_number(), NA)) %>% 
  fill(flag, .direction = "up") %>% 
  filter(x != "") %>% 
  mutate(x = as.numeric(x)) %>% 
  group_by(flag) %>% 
  group_split()


select_and_add <- function(.x){
  .x %>% 
    select(x) %>% 
    summarise(sum = sum(x)) %>% 
    pull(sum)
}

elf_sums <- input_list %>% 
  map_dbl(.f = select_and_add) 

tibble(
  calories = elf_sums
) %>% 
  arrange(desc(calories)) %>% 
  head(3)




