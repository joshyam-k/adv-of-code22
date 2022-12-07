library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day7.txt")))

# only know the size of the files in a directory if we cd to it



move <- function(fpath, curr_dir) {
  if(!is.na(curr_dir)) {
    if(curr_dir == "..") {
      # need to cut off last /directory_name from path
      fpath <- fpath[-length(fpath)]
      return(fpath)
    }
    fpath <- c(fpath, paste0(fpath[length(fpath)], "/", curr_dir))
    return(fpath)
  }
  return(fpath)
}




# use separate rows and change what move returns

counts <- input %>% 
  mutate(x = case_when(
    x == "$ cd /" ~ "$ cd base",
    T ~ x
  )) %>%
  mutate(dir = str_extract(x, "cd (.*)"),
         dir = str_remove(dir, "cd\\s")) %>%
  mutate(path = accumulate(dir, move)) %>% 
  select(-dir) %>% 
  unnest(path) %>% 
  filter(str_detect(x, "\\d")) %>%
  mutate(x = parse_number(x)) %>% 
  group_by(path) %>%
  summarise(size = sum(x)) %>% 
  ungroup()
  
counts %>% 
  filter(size < 100000) %>% 
  summarise(sum(size))


total <- 70000000
needed <- 30000000
used <- 42586708

counts %>% 
  filter(size > (used + needed - total)) %>% 
  summarise(min(size))






