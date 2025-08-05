source("get_aoc_input.R")
library(tidyverse)

raw_input <- get_aoc(2) |> str_split("\n") |> pluck(1)

input <- map(raw_input,\(x) str_split(x," ") |> unlist()|>  as.numeric())

safe_flag <- function(report){
  #adjacent diffs
  Diff <- map(seq(1,length(report)-1),\(x) report[x+1]-report[x]) |> unlist()
  #monotonous check
  Monotonous_Flag <- all(sign(Diff)==1) | all(sign(Diff)==-1)
  
  #adjacent level diff where 3 => x >= 1
  if(max(abs(Diff))<=3 & min(abs(Diff))>=1){Adj_Flag <- TRUE}  else{Adj_Flag <- FALSE}
  return(Monotonous_Flag*Adj_Flag)
}

part_1 <- map(input[!is.na(input)],\(x) safe_flag(x)) |> unlist() |> sum()
#606
