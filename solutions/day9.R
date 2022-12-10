library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day9.txt"))) %>% 
  separate(x, into = c("direction", "size"), sep = " ", convert = T) %>% 
  uncount(size)

move <- tibble(
  inp = c('L', 'R', 'U', 'D'),
  x_d = c(-1, 1, 0, 0),
  y_d = c(0, 0, 1, -1)
)


# tail and head are vectors of length 2
tail_follow <- function(tail, head){
  dist <- sqrt((tail[1] - head[1])^2 + (tail[2] - head[2])^2)
  if(dist <= sqrt(2)){
    # in other words don't move the tail
    return(tail)
  }
  return(tail + sign(head - tail))
}

p1 <- input %>% 
  left_join(move, by = c("direction" = "inp")) %>% 
  mutate(head_x = cumsum(x_d),
         head_y = cumsum(y_d)) %>% 
  mutate(head = map2(head_x, head_y, c)) %>% 
  mutate(tail1 = accumulate(head, tail_follow, .init = c(0,0))[-1]) 

p1 %>% 
  rowwise() %>% 
  mutate(tail_x = tail9[1], tail_y = tail9[2]) %>% 
  distinct(tail_x, tail_y) %>% 
  nrow()

for(i in 2:9){
  p1[[paste0("tail", i)]] <- accumulate(p1[[paste0("tail", i - 1)]],
                                       tail_follow,
                                       .init = c(0,0))[-1]
}
 

