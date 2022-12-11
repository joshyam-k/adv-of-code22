library(tidyverse)
library(here)


input <- tibble(x = readLines(here("inputs", "day11.txt"))) %>% 
  filter(x != "") %>% 
  mutate(extr = case_when(
    str_detect(x, "Operation") ~ str_extract(x, "(?<=(=\\s))(.*)"),
    str_detect(x, "Starting") ~ str_extract(x, "(?<=(:\\s))(.*)"),
    T ~ as.character(parse_number(x))
  )) %>% 
  mutate(extr = str_replace_all(extr, "old", "x"),
         id = sort(rep(0:7,6)))

# going to have to be careful with indexing...
# switching to base 1 indexing because of R

monkey_list <- list()
for(i in 0:7){
  temp <- input %>% 
    filter(id == i)
  
  # function factory helps with forcing evaluation
  raw_f <- temp$extr[3]
  extr <- temp$extr
  
  op <- function(exp) {
    force(exp)
    function(x) {
      eval(parse(text = exp))
    }
  }
  
  tester <- function(exp2) {
    force(exp2)
    function(x){
      ifelse(
        x %% as.numeric(exp2[4]) == 0, 
        as.numeric(exp2[5]) + 1,
        as.numeric(exp2[6]) + 1
      )
    }
  }
  monkey <- list(
    id = i + 1,
    starting_items = as.numeric(str_split(extr[2], ",\\s")[[1]]),
    operation = op(raw_f),
    test = tester(extr)
  )
  
  monkey_list[[i + 1]] <- monkey
  
}


## part 1
### 20 rounds

inspections <- c(0, 0, 0, 0, 0, 0, 0, 0)


# rounds
for(i in 1:20){
  # monkeys
  for(j in 1:8){
    m <- monkey_list[[j]]
    items <- m$starting_items
    if(length(items) == 0) {
      next
    }
    # items
    for(k in 1:length(items)) {
      new <- m$operation(items[k])
      to_add <- round(new/3)
      give_to <- m$test(to_add)
      monkey_list[[give_to]]$starting_items <- c(monkey_list[[give_to]]$starting_items, new)
      inspections[j] <- inspections[j] + 1
      
    }
    monkey_list[[j]]$starting_items <- c()
  }

}

sort(inspections)





