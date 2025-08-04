source("get_aoc_input.R")
library(tidyverse)

raw_input <- get_aoc(2) |> str_split("\n") |> pluck(1)

input <- map(raw_input,\(x) str_split(x," ") |> unlist()|>  as.numeric())
temp <- input |> pluck(1) |> unlist()
map(seq(1,length(temp)-1),\(x) temp[x+1]-temp[x]) |> unlist() 
map(seq_along(temp),\(x) temp[x]-temp[x-1]) |> unlist()

safe_flag <- function(report){
  #adjacent diffs
  Diff <- map(seq(1,length(report)-1),\(x) report[x+1]-report[x]) |> unlist()
  #monotonous check
  Monotonous_Flag <- all(sign(Diff)==1) | all(sign(Diff)==-1)
  
  #adjacent level diff where 3 => x >= 1
  if(max(Diff)<=3 & min(Diff)){Adj_Flag <- TRUE}  else{Adj_Flag <- FALSE}
  return(Monotonous_Flag*Adj_Flag)
}


part_1 <- map(input[!is.na(input)],\(x) safe_flag(x)) |> unlist() |> sum()
#651 - wrong, should be 606
#Fix
input |> mutate( 
  Diff = map(diff),
  Monotonous_Flag <- all(sign(Diff)==1) | all(sign(Diff)==-1))

