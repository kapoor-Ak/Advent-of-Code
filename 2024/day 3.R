source("get_aoc_input.R")
library(tidyverse)

raw_input <- get_aoc(3) |> str_split("\n")

regex_extract <- function(expr){
  pattern <- "mul\\(\\d{1,3}\\,\\d{1,3}\\)"
  temp <- str_extract_all(expr,pattern) |> unlist()
  regex_tbl <- tibble(mult_expr=temp)
  regex_tbl |> mutate(num_1 = str_replace(mult_expr,"^mul\\((\\d{1,3})\\,\\d{1,3}\\)$","\\1") |> as.numeric(),
                      num_2 = str_replace(mult_expr,"^mul\\(\\d{1,3}\\,(\\d{1,3})\\)$","\\1") |> as.numeric(),
                      mult = num_1*num_2)
}
part_1 <- regex_extract(raw_input) |> select(mult) |> sum()
#> 187825547
