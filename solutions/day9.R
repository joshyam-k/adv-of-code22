library(tidyverse)
library(here)

raw <- tibble(x = readLines(here("inputs", "day9.txt")))