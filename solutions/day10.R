library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day10.txt"))) %>% 
  separate(x, into = c("instr", "val"), sep = " ", convert = T) %>% 
  mutate(val = replace_na(val, 0)) %>% 
  mutate(count = ifelse(instr == "addx", 2, 1)) %>% 
  uncount(count, .id = "id") %>% 
  mutate(cycle = row_number(),
         val = ifelse(cycle == 1, 1, val)) %>% 
  mutate(val = case_when(
    (instr == "addx") & (id == 1) ~ 0,
    T ~ val
  )) %>% 
  mutate(
    val = lag(cumsum(val), k = 1, default = 1)
    )


## part 1

input %>% 
  filter(cycle %in% c(20, 60, 100, 140, 180, 220)) %>% 
  summarise(sum(val*cycle))


## part 2

input %>% 
  mutate(
    row = (cycle -1) %/% 40,
    col = (cycle -1) %% 40,
    pixel = ifelse(abs(col - val) <= 1, '#', '.')
  ) %>% 
  group_by(row) %>% 
  summarise(paste0(pixel, collapse = ""))





