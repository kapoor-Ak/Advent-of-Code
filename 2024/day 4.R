source("get_aoc_input.R")
library(tidyverse)
library(R6)

raw_input <- get_aoc(4) |> str_split("\n") |> pluck(1) |> head(-1)

# constructing a R6 class "Scrambler" to solve this to learn more about OOP

temp1 <- "XMASAMXXXMAS"

pattern1 <- "XMAS"
pattern2 <- "SAMX"
pattern <- paste0("(", pattern1, "|", pattern2, ")")

str_extract_all(temp1,c(pattern1,pattern2))|> unlist() |> length()

scrambler <- R6Class("scrambler",
                     public = list(
                       pattern = c("",""),
                       
                       initialize = function(x){
                         
                       },
                       solver = function(x){
                         SUM <- seself$puzzle
                       }),
                       private = list(
                       horizontal = function(x) {
                         
                       },
                       vertical = function(x) {
                         
                       },
                       fwd_slash = function(x) {
                         
                       },
                       bwd = function(x) {
                         
                       }
                     ))

# MMMSXXMASM
# MSAMXMSMSA
# AMXSXMAAMM
# MSAMASMSMX
# XMASAMXAMM
# XXAMMXXAMA
# SMSMSASXSS
# SAXAMASAAA
# MAMMMXMMMM
# MXMXAXMASX
# rough
sample1 <- c(
  "MMMSXXMASM",
  "MSAMXMSMSA",
  "AMXSXMAAMM",
  "MSAMASMSMX",
  "XMASAMXAMM",
  "XXAMMXXAMA",
  "SMSMSASXSS",
  "SAXAMASAAA",
  "MAMMMXMMMM",
  "MXMXAXMASX"
)

temp <- raw_input |> str_split("") |> unlist() |> matrix(nrow = length(raw_input),byrow = TRUE) # matrix creation works 

pattern1 <- "XMAS"
pattern2 <- "SAMX"
pattern <- paste0("(", pattern1, "|", pattern2, ")")

horizontal <- function(df) {
  map(1:nrow(df), \(x) df[x,] |> str_flatten()) |> 
    map(~ str_extract_all(.x, pattern)) |> 
    unlist() |> length()
}

vertical <- function(df) {
  map(1:ncol(df), \(x) df[,x] |> str_flatten()) |> 
    map(~ str_extract_all(.x, pattern)) |> 
    unlist() |> length()
}
bwd <- function(df) { # "\"
  idx <- map(-(nrow(df)-1):(nrow(df)-1), \(k)
             which(row(df) - col(df) == k, arr.ind = TRUE)
  ) |> keep(~ length(.x) >= nchar("XMAS"))
  
  map(idx, \(coords)
      str_flatten(df[coords]) |> str_extract_all(pattern)
  ) |> unlist() |> length()
}

fwd <- function(df) { # "/"
  idx <- map(2:(2*nrow(df)), \(k)
             which(row(df) + col(df) == k, arr.ind = TRUE)
  ) |> keep(~ length(.x) >= nchar("XMAS"))
  
  map(idx, \(coords)
      str_flatten(df[coords]) |> str_extract_all(pattern)
  ) |> unlist() |> length()
}
horizontal(temp)
vertical(temp)
fwd(temp)
bwd(temp)
horizontal(temp)+vertical(temp)+fwd(temp)+bwd(temp)

