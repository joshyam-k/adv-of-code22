library(tidyverse)
library(here)

raw <- tibble(x = readLines(here("inputs", "day8.txt"))) %>% 
  mutate(x = str_split(x, ""))

input <- matrix(rep(0, 9801), ncol = 99)
for(i in 1:99){
  input[i, ] <- as.numeric(raw$x[[i]])
}

## part 1
## automatically have 392 for free

total <- 392
for(row in 2:98){
  for(col in 2:98){
    value <- input[row, col]
    top <- head(input[ , col], row - 1)
    bottom <- tail(input[ , col], 99 - row)
    left <- head(input[row ,], col - 1)
    right <- tail(input[row ,], 99 - col)
    taller <- all(top < value) |  all(bottom < value) |  all(left < value) | all(right < value)
    total <- total + sum(taller)
  }
}


## part 2

# 0 0 0 0 
# 0 0 0 0
# 0 X 0 0
# 0 0 0 0

dist_count <- function(x, row, col, direction) {
  id <- which(x >= value)
  if(identical(id, integer(0))) {
    if(direction == "top") {
      return(row - 1)
    } else if(direction == "down"){
      return(99 - row)
    } else if(direction == "left") {
      return(col - 1)
    } else {
      return(99 - col)
    }
  }
  if(direction == "top"){
    return(row - max(id))
  } else if(direction == "bottom") {
    return(min(id))
  } else if(direction == "left") {
    return(col - max(id))
  } else if(direction == "right") {
    return(min(id))
  }
}

max_score <- 0
for (row in 2:98) {
  for (col in 2:98) {
    value <- input[row, col]
    top <- head(input[ , col], row - 1)
    bottom <- tail(input[ , col], 99 - row)
    left <- head(input[row ,], col - 1)
    right <- tail(input[row ,], 99 - col)
    score <- dist_count(top, row, col, "top")*dist_count(bottom, row, col, "bottom")*
      dist_count(left, row, col, "left")*dist_count(right, row, col, "right")
    if(score > max_score) {
      max_score <- score
    }
  }
}


  
