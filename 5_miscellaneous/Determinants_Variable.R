###########################################
# Determinants_Variable
###########################################

library(tidyverse)
library(fs)

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

#######################################################
# For the Determinants
#######################################################

# for loop over the files
for(i in seq_along(file_contents)){
  if(i == 1){
    Leverage  <- t(t(file_contents[[i]][,1]))
    Leverage2 <- t(t(file_contents[[i]][,2]))
    Size <- t(t(file_contents[[i]][,3]))
    Size2 <- t(t(file_contents[[i]][,4]))
    Capitalization <- t(t(file_contents[[i]][,5]))
    Loss_ra <- t(t(file_contents[[i]][,6]))
    Expens_ra <- t(t(file_contents[[i]][,7]))
    ROE <- t(t(file_contents[[i]][,8]))
    OWNER <- t(t(file_contents[[i]][,9]))
    Herfin <- t(t(file_contents[[i]][,10]))
    Combine_ra <- t(t(file_contents[[i]][,11]))
    Liqu <- t(t(file_contents[[i]][,12]))
    Control <- t(t(file_contents[[i]][,13]))
  }else{
    Leverage <- cbind(Leverage, t(t(file_contents[[i]][,1])))
    Leverage2 <- cbind(Leverage2, t(t(file_contents[[i]][,2])))
    Size <- cbind(Size, t(t(file_contents[[i]][,3])))
    Size2 <- cbind(Size2, t(t(file_contents[[i]][,4])))
    Capitalization <- cbind(Capitalization, t(t(file_contents[[i]][,5])))
    Loss_ra <- cbind(Loss_ra, t(t(file_contents[[i]][,6])))
    Expens_ra <- cbind(Expens_ra, t(t(file_contents[[i]][,7])))
    ROE <- cbind(ROE, t(t(file_contents[[i]][,8])))
    OWNER <- cbind(OWNER, t(t(file_contents[[i]][,9])))
    Herfin <- cbind(Herfin, t(t(file_contents[[i]][,10])))
    Combine_ra <-  cbind(Combine_ra, t(t(file_contents[[i]][,11])))
    Liqu <- cbind(Liqu, t(t(file_contents[[i]][,12])))
    Control <- cbind(Control, t(t(file_contents[[i]][,13])))
  }
}
# Write tables with the results
write.table(Leverage, file = "Leverage.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Leverage2, file = "Leverage2.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Size, file = "Size.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Size2, file = "Size2.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Capitalization, file = "Capitalization.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Loss_ra, file = "Loss_ra.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Expens_ra, file = "Expens_ra.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(ROE, file = "ROE.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(OWNER, file = "OWNER.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Herfin, file = "Herfin.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Combine_ra, file = "Combine_ra.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Liqu, file = "Liqu.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Control, file = "Control.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)

#######################################################
# For the NBSM variables
#######################################################

# for loop over the files
for(i in seq_along(file_contents)){
  if(i == 1){
    Labor <- t(t(file_contents[[i]][,1]))
    Debt <- t(t(file_contents[[i]][,2]))
    Equity <- t(t(file_contents[[i]][,3]))
    Incurred_claims <- t(t(file_contents[[i]][,4]))
    Investment <- t(t(file_contents[[i]][,5]))
    Profits <- t(t(file_contents[[i]][,6]))
    Av_Invested <- t(t(file_contents[[i]][,7]))
  }else{
    Labor <- cbind(Labor, t(t(file_contents[[i]][,1])))
    Debt <- cbind(Debt, t(t(file_contents[[i]][,2])))
    Equity <- cbind(Equity ,t(t(file_contents[[i]][,3])))
    Incurred_claims <- cbind(Incurred_claims, t(t(file_contents[[i]][,4])))
    Investment <- cbind(Investment, t(t(file_contents[[i]][,5])))
    Profits <- cbind(Profits, t(t(file_contents[[i]][,6])))
    Av_Invested <- cbind(Av_Invested, t(t(file_contents[[i]][,7])))
  }
}
# Write tables with the results
write.table(Labor, file = "Labour.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Debt, file = "Debt.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Equity, file = "Equity.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Incurred_claims, file = "Incurred_claims.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Investment, file = "Investment.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Profits, file = "Profits.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(Av_Invested, file = "Av_Invested.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)