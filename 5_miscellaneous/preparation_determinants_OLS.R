###########################################
# preparation_determinants_OLS
###########################################

library(tidyverse)
library(fs)
library(psych)

Path <- "C:/xxxx"

# Control matrix für non-acquirer
Path_con <- "C:/xxxx.csv"
Control <- read.csv(file = Path_con, header=TRUE, sep = ";");

file_paths <- fs::dir_ls(Path)
# For loop: read files
file_contents <- list();
for(i in seq_along(file_paths)){
  # read the files of the file_paths and save them into file_contents
  file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
}
# Rename the file names in the list file_contents with the paths in file_paths:
file_contents <- set_names(file_contents, file_paths)

# for loop over all the files
for(i in seq_along(file_contents)){
  # for loop over all the columns in the file
  for(j in 1:7){
    a <- file_contents[[i]][,j]
    b <- Control[,j]
    df <- data.frame(a,b)
    # filter non-acquirer
    # filter acquirer before excl.
    acq_b_excl <- filter(df, b == 1 | b == 2)
    # filter acquirer after excl.
    acq_a_excl <- filter(df, b == 1)
    # Combine all the ten years together:
    if(j == 1){
      res_acq_b_excl <- t(t(acq_b_excl[,1]));
      res_acq_a_excl <- t(t(acq_a_excl[,1]));
    }else{
      res_acq_b_excl <- rbind(res_acq_b_excl, t(t(acq_b_excl[,1])));
      res_acq_a_excl <- rbind(res_acq_a_excl, t(t(acq_a_excl[,1])));
    }
  }
  H2 <-paste("det_acq_b", "_", toString(i),".csv", sep="")
  write.table(res_acq_b_excl, file = H2, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
  H3 <-paste("det_acq_a", "_", toString(i),".csv", sep="")
  write.table(res_acq_a_excl, file = H3, row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
}