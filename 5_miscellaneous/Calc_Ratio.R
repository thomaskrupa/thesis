###########################################
# Calc_Ratio
###########################################

library(tidyverse)
library(fs)
library(psych)
library(car)

setwd("C:/xxxx")

################################################
# Life
################################################

###### Lfe CRS
# Path <- "C:/xxxx"

###### Lfe VRS
# Path <- "C:/xxxx"

################################################
# Non-life
################################################

###### Non-life CRS
# Path <- "C:/xxxx"

###### Non-life VRS
# Path <- "C:/xxxx"

################################################
# Scale efficiency ratios
################################################

# Scale efficiency life
 #Path <- "C:/xxxx"

# Scale efficiency non-life
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

#life : 115
#non-life: 216

# res is the final list
res <- matrix(0,216,7)

# for loop over all the files
for(i in seq_along(file_contents)){
  # for loop over all the columns in the file
  for(j in 1:7){
    res[,j] <- file_contents[[i]][,j+3] / file_contents[[i]][,j]
  }
  H1 <-paste("ratio_eff_", "_", toString(i),".csv", sep="")
  write.table(res, file = H1, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
}