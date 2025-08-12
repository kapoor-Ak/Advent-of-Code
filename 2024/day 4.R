source("get_aoc_input.R")
library(tidyverse)
raw_input <- get_aoc(4) |> str_split("\n") |> pluck(1) |> head(-1)

input <- raw_input |> str_split("") |> unlist() |> matrix(nrow = length(raw_input),byrow = TRUE) # matrix creation works 
#part1
horizontal <-  function(df) {
  map(1:(nrow(df)),\(x) df[x,] |> str_flatten() |> str_extract_all(,pattern=c("XMAS","SAMX"))) |> unlist() |> length()}

vertical <- function(df){
  df |> t() |> horizontal()
}
bwd <- function(df){#\
  idx <- map(-nrow(df):nrow(df),\(x) which(row(df) - col(df) == x, arr.ind = TRUE))
  map(idx,\(x) df[x]|> str_flatten()|> str_extract_all(pattern=c("XMAS","SAMX")))|> unlist() |>na.omit()|> length()
      }
fwd <- function(df){ df |> apply(2,rev)|> t() |> bwd()}
#part2 The current way is slow as it creates all possible 3x3 and checks it, may be able to find a better way in future
X_mas <- function(df){
  all(c(any(diag(df) |> str_flatten() |>  str_detect(,pattern=c("MAS","SAM"))==1),
        any(df |> t() |> apply(2,rev) |> diag()|> str_flatten() |> str_detect(,pattern=c("MAS","SAM"))==1))==1)
}
part2_func <- function(df){
  positions <- expand.grid(row = 1:(nrow(input)-2), col = 1:(ncol(input)-2))
  sub <- pmap(positions, function(row, col) input[row:(row+2), col:(col+2)])
  map(seq_along(sub),\(x) sub[[x]] |> X_mas()) |> unlist() |> sum()
} 

part1 <- horizontal(input)+vertical(input)+fwd(input)+bwd(input) #2496
part2 <- part2_func(input) #1967

