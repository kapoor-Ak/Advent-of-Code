source("get_aoc_input.R")
library(tidyverse)

data <- get_aoc(1) |>str_extract_all("[^\\n]+") |> unlist()


remainder <- function(a,b,sign_x){
  t <- a +b*sign_x
  status <-  (t/100 - trunc(t/100))*100
  status <- ifelse(status < 0, status + 100, status) |> round(2)
  counter <- case_when( t > 100 ~ floor(t/100) + 1,
                        t < 0 ~ floor(t/100)*-1,
                        #((t%%100)/100 + floor(t/100)) == 0 ~ -1,
                        TRUE ~ 0)
  return(c(status,counter))
}

f <- function(t){
  status <- 50
  count <- 0
  count2 <- count
  x <- c()
  y <- x
  for (i in t) {
    t2 <- str_remove(i,"L|R") |> as.numeric()
    sign_x <- str_remove(i,"\\d+")
    sign_x <-  ifelse(sign_x == "L", -1, 1)
    output <- remainder(status,t2,sign_x)
    status <- output[1]
    count2 <- count2 + output[2]
    if(status%%100 == 0){
      count = count + 1
    }
  }
  return(c(count,count2))
}

output <- f(data)
# part 1

part1 <- output[1];part1

#1,066

part2 <- output[2];part2

#6,223


