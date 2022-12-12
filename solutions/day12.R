library(tidyverse)
library(here)
library(adventdrob)
library(tidygraph)

raw <- tibble(x = readLines(here("inputs", "day12.txt")))  %>% 
  grid_graph(x, mutual = T, directed = T) 

nodes <- raw %>% 
  as_tibble() %>% 
  mutate(elevation = case_when(
    value == 'S' ~ 1L,
    value == 'E' ~ 26L,
    T ~ match(value, letters)
  ))

input <- raw %>% 
  activate("edges") %>% 
  mutate(from_val = nodes$elevation[from],
         to_val = nodes$elevation[to]) %>% 
  filter(to_val <= from_val + 1) %>% 
  activate("nodes") %>% 
  mutate(distance = node_distance_to(which(nodes$value == 'E'), mode = "out"))

input %>% 
  filter(value == 'S')

input %>% 
  filter(value == 'a') %>% 
  arrange(distance)
