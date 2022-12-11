library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day11.txt")))