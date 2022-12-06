library(tidyverse)
library(here)

input <- readLines(here("inputs", "day6.txt"))


done <- F
start <- 1
end <- 4
while(!done) {
  to_check <- str_sub(input, start, end)
  n_unique <- length(unique(str_split(to_check, "")[[1]]))
  if(n_unique == 4) {
    done <- T
  } else {
    start <- start + 1
    end <- end + 1
  }
}

## part 2 just change end to 14 and (n_unique == 14)
