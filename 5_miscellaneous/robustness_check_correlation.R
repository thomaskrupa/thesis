##########################################
# Robustness check (life, non-life)
##########################################

library(tidyverse)
library(fs)
library(psych)

# Path for the solution
setwd("C:/xxxx")

# Life
# path of the date efficiency scores with incurred benefits
# Path <- "C:/Users/tkrup/Documents/phd thesis/11_Robustness_CHECK_corr/life/1_incurred"

# Non- Life
# path of the date efficiency scores with incurred claims
 Path <- "C:/xxxx"

file_paths <- fs::dir_ls(Path)
# For loop: read files
file_contents <- list();
for(i in seq_along(file_paths)){
  # read the files of the file_paths and save them into file_contents
  file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
}
# Rename the file names in the list file_contents with the paths in file_paths:
file_contents <- set_names(file_contents, file_paths)
EF_inc <- file_contents;

# Life
# path of the date efficiency scores with earned premium
# Path <- "C:/xxxx"

# Non-Life
# path of the date efficiency scores with earned premium
 Path <- "C:/xxxx"

file_paths <- fs::dir_ls(Path)
# For loop: read files
file_contents <- list();
for(i in seq_along(file_paths)){
  # read the files of the file_paths and save them into file_contents
  file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
}
# Rename the file names in the list file_contents with the paths in file_paths:
file_contents <- set_names(file_contents, file_paths)
EF_prem <- file_contents;

# Calculation of the correlation...
# Change the number of columns from 1 to 7 step by step
x <- EF_inc[[7]]
x <- t(t(x))

y <- EF_prem[[7]]
y <- t(t(y))

cor.test(x,y, method ="spearman")