source("get_aoc_input.R")
library(tidyverse)
library(stringr)

raw_input <- get_aoc(3) |> str_split("\n")

regex_extract <- function(expr){
  pattern <- "mul\\(\\d{1,3}\\,\\d{1,3}\\)"
  temp <- str_extract_all(expr,pattern) |> unlist()
  regex_tbl <- tibble(mult_expr=temp)
  regex_tbl |> mutate(num_1 = str_replace(mult_expr,"^mul\\((\\d{1,3})\\,\\d{1,3}\\)$","\\1") |> as.numeric(),
                      num_2 = str_replace(mult_expr,"^mul\\(\\d{1,3}\\,(\\d{1,3})\\)$","\\1") |> as.numeric(),
                      mult = num_1*num_2)
}
part_1 <- raw_input |> regex_extract() |> select(mult) |> sum()
#> 187825547

part2_func <- function(expr){
  do_ind <- str_locate_all(raw_input,"do\\(\\)") |> pluck(1) |> as_tibble() |> mutate(flag =TRUE)
  dont_ind <- str_locate_all(raw_input,"don't\\(\\)")|> pluck(1)|> as_tibble()|> mutate(flag =FALSE)
  ind_df <- bind_rows(do_ind,dont_ind) |> 
   select(start,flag) |> 
    add_row(start = 1, flag = TRUE) |> 
    arrange(start) |> 
    mutate(end = c(start[-1]-1,nchar(raw_input))) |> 
    filter(flag==TRUE)
  return(ind_df)
}

Index <- part2_func(raw_input)

part_2  <- map(1:nrow(Index),\(x) str_sub(raw_input,Index[x,"start"],Index[x,"end"]) |>regex_extract()) |> bind_rows() |> 
  select(mult) |> sum()
#>85508223

#Notes for improvement
#> Learn how to use "Look-Arounds" and "Alternates" in the stringr package
#> didnt need to create a data frame for the 1st part could have just added one more str_extract() on temp and to get prod() there
