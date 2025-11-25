source("get_aoc_input.R")
library(tidyverse)
raw_input <- get_aoc(5) |> str_split("\n") |> pluck(1) |> head(-1)
order <- str_extract_all(raw_input, "\\d{2}\\|\\d{2}") |> unlist()
testing <- raw_input[(length(order)+1):length(raw_input)] |> tail(-1)

order_df <- data.frame(col1 = order)|> 
  tidyr::separate(col1, into = c("before", "after"), sep = "\\|") |>
  mutate(before = as.numeric(before), after = as.numeric(after))

middle_element <- function(v) {
  v[median(seq_along(v))] |>
    as.character() |>
    as.numeric()
}

testing_df <- testing |> 
  strsplit(",") |> 
  lapply(as.numeric)

order_check <- function(array, list_df){
  array <- as.numeric(array)
  for (i in length(array):1) {
    temp <- list_df |> filter(before == array[i]) |> pluck("after")
    continuance_flag <- (array[1:(i-1)] %in% temp)|> any()
    if (continuance_flag) {
      return(0)
    }
  }
  return(middle_element(array))
}

part_1 <- map((1:length(testing_df)),\(x) order_check(testing_df[[x]],list_df = order_df) ) |> 
  unlist() |> sum()
#5091

#Topological sort 
topological_sort <- function(array, order_df) {
  relevant_rules <- order_df |>
    filter(before %in% array & after %in% array)
  
  result <- c()
  remaining <- array
  
  while (length(remaining) > 0) {
    candidates <- remaining[sapply(remaining, function(x) {
      sum(relevant_rules$after == x & relevant_rules$before %in% remaining) == 0
    })]
    
    if (length(candidates) == 0) break
    
    next_num <- candidates[1]
    result <- c(result, next_num)
    remaining <- setdiff(remaining, next_num)
    relevant_rules <- relevant_rules |> filter(before != next_num)
  }
  
  sorted <- c(result, remaining)
  return(sorted)
}
testing_df <- testing_df[which(map((1:length(testing_df)),\(x) order_check(testing_df[[x]],list_df = order_df) ) |> 
  unlist() == 0)]

part_2 <- purrr::map((1:length(testing_df)),\(x) topological_sort(testing_df[[x]],order_df = order_df) |> middle_element() ) |> 
  unlist() |> sum()

#4681
