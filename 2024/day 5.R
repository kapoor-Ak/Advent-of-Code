source("get_aoc_input.R")
library(tidyverse)
raw_input <- get_aoc(5) |> str_split("\n") |> pluck(1) |> head(-1)
order <- str_extract_all(raw_input, "\\d{2}\\|\\d{2}") |> unlist()
testing <- raw_input[(length(order)+1):length(raw_input)] |> tail(-1)

#to be continued ...

