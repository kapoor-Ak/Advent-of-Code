source("get_aoc_input.R")
library(tidyverse)



data <- get_aoc(1) |> str_extract_all("\\d+")

#> get_aoc() function
#> needs to be fixed probably as this gets the output for day 1 as 
#> "19394   30201\n88523   40612\n60736", rather than in two separate columns


temp <- pluck(data,1)

first <- temp[seq(1, length(temp), by = 2)] |> 
        unlist() |> as.numeric()

second <- temp[seq(2, length(temp), by = 2)] |>
          unlist() |> as.numeric()


# part 1
abs(sort(first)-sort(second))|> sum()
#2,742,123


# part 2
map(first, \(x) x*length(which(second==x)))|>unlist()|>sum() # learning how to use map function, referred this, work a new solution
#2,1328,497

