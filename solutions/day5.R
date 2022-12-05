library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day5.txt"))) %>% 
  mutate(n = str_extract(x, "(?<=move\\s)\\d+"),
         src = str_extract(x, "(?<=from\\s)\\d+"),
         dest = str_extract(x, "(?<=to\\s)\\d+")) %>% 
  select(-x) %>% 
  mutate_if(is.character, as.numeric)

stack <- list(
  c("B", "V", "S", "N", "T", "C", "H", "Q"),
  c("W", "D", "B", "G"),
  c("F", "W", "R", "T", "S","Q", "B"),
  c("L", "G", "W", "S", "Z", "J", "D", "N"),
  c("M", "P", "D", "V", "F"),
  c("F", "W", "J"),
  c("L", "N", "Q", "B", "J", "V"),
  c("G", "T", "R", "C", "J", "Q", "S", "N"),
  c("J", "S", "Q", "C", "W", "D", "M")
)

for(i in 1:nrow(input)) {
  vals <- unlist(input[i, ])
  stack[[vals[3]]] <- c(stack[[vals[3]]], rev(tail(stack[[vals[2]]], vals[1])))
  stack[[vals[2]]] <-  head(stack[[vals[2]]], length(stack[[vals[2]]]) - vals[1])
}

stack %>% 
  map_chr(last)


## part2: just remove rev from the above code






