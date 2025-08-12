source("get_aoc_input.R") # solution incomplete to fix
library(tidyverse)

raw_input <- get_aoc(4) |> str_split("\n") |> pluck(1) |> head(-1)

temp <- raw_input |> str_split("") |> unlist() |> matrix(nrow = length(raw_input),byrow = TRUE) # matrix creation works 

pattern1 <- "XMAS"
pattern2 <- "SAMX"
pattern_use <- paste0("(", pattern1, "|", pattern2, ")")

horizontal <-  function(df) {
  map(1:(nrow(df)),\(x) df[x,] |> str_flatten() |> str_extract_all(,pattern=c("XMAS","SAMX"))) |> unlist() |> length()}

vertical <- function(df){
  df |> t() |> horizontal()
}
bwd <- function(df){#\
  idx <- map(-nrow(df):nrow(df),\(x) which(row(df) - col(df) == x, arr.ind = TRUE))
  map(idx,\(x) df[x]|> str_flatten()|> str_extract_all(pattern=c(pattern1,pattern2)))|> unlist() |>na.omit()|> length()
      }
fwd <- function(df){
  df |> apply(2,rev)|> t() |> bwd()
}

part1 <- horizontal(temp)+vertical(temp)+fwd(temp)+bwd(temp) #2496


