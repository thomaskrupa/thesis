##########################################
# OLS regression (life) and SE
##########################################

library(tidyverse)
library(fs)
library(psych)
library(stargazer) # R to latex
library (car) # vif function
library("Hmisc") # Using rcorr
library(Routliers)
library(broom)
library(olsrr) # assumption check for ols
library(MASS)
library(lmtest) # Packages for Breusch-Pagan test
library(sandwich)

# Results path
setwd("C:/xxxx")

##########################################
# Life SE
# before excl. import
##########################################

# Path of the data
Path <- "C:/xxxx"
# Control matrix forr non-acquirer (Path of the file)
Path_det <- "C:/xxxx.csv"
det_b <- read.csv(file = Path_det, header=TRUE, sep = ";");

file_paths <- fs::dir_ls(Path)
# For loop: read files
file_contents <- list();
for(i in seq_along(file_paths)){
  # read the files of the file_paths and save them into file_contents
  file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
}
# Rename the file names in the list file_contents with the paths in file_paths:
file_contents <- set_names(file_contents, file_paths)
# efficiency scores:
OE_b <- file_contents[[1]]

##########################################
# Life SE
# after excl. import
##########################################

# Path of the data
Path <- "C:/xxxx"
# Control matrix forr non-acquirer (path of the file)
Path_det <- "C:/xxxx.csv"
det_a <- read.csv(file = Path_det, header=TRUE, sep = ";");
file_paths <- fs::dir_ls(Path)
# For loop: read files
file_contents <- list();
for(i in seq_along(file_paths)){
  # read the files of the file_paths and save them into file_contents
  file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
}
# Rename the file names in the list file_contents with the paths in file_paths:
file_contents <- set_names(file_contents, file_paths)
# efficiency scores:
OE_a <- file_contents[[1]]

##########################################
# data frames
##########################################

# before excl.:
data_oe_b <- data.frame(OE_b, det_b)
# after excl.:
data_oe_a <- data.frame(OE_a, det_a)

##########################################
# Outlier detection
########################################## 

# threefold parameter
tf= 2

# data_oe_b
OE_b <- t(OE_b)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(OE_b) - tf*mad(OE_b)
upper_bound <- median(OE_b) + tf*mad(OE_b)
# Place of the outliers
outlier_ind <- which(OE_b < lower_bound | OE_b > upper_bound)
# delete rows
data_oe_b <- data_oe_b[-outlier_ind,]

# data_oe_a
OE_a <- t(OE_a)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(OE_a) - tf*mad(OE_a)
upper_bound <- median(OE_a) + tf*mad(OE_a)
# Place of the outliers
outlier_ind <- which(OE_a < lower_bound | OE_a > upper_bound)
# delete rows
data_oe_a <- data_oe_a[-outlier_ind,]

##########################################
# Transformation into dataframe
##########################################

# before
det_oe_b_alt <- data_oe_b
# after
det_oe_a_alt <- data_oe_a
# before
det_oe_b <- data_oe_b[,-1]
# after
det_oe_a <- data_oe_a[,-1] 

##########################################
# Check normal distribution:
##########################################

# Overall effiency
# acquisition before excl.
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 2)
res_p <- matrix(0, 6, 2)
# perform a for loop
for(i in 1:6){
  # Check normal distribution with the Kolmogorov-Smirnov test and save it into res
  KS <- ks.test(det_oe_b[,i],"pnorm", mean(det_oe_b[,i]), sd(det_oe_b[,i]))
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_oe_b[,i])
  # Save d statistics into res
  res[i,1] <- round(KS$statistic,5)
  res[i,2] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- KS$p.value
  res_p[i,2] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_KS_SW_acq_SE_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_KS_SW_p_value_acq_SE_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 2)
res_p <- matrix(0, 6, 2)
# perform a for loop
for(i in 1:6){
  # Check normal distribution with the Kolmogorov-Smirnov test and save it into res
  KS <- ks.test(det_oe_a[,i],"pnorm", mean(det_oe_a[,i]), sd(det_oe_a[,i]))
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_oe_a[,i])
  # Save d statistics into res
  res[i,1] <- round(KS$statistic,5)
  res[i,2] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- KS$p.value
  res_p[i,2] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_KS_SW_acq_SE_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_KS_SW_p_value_acq_SE_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

##########################################
# Correlation matrix according Spearman
##########################################

# Overall efficiency
# acquisition before excl.
# Calculate the correlation matrix according Spearman
B <- rcorr(as.matrix(det_oe_b), type = "spearman")
# Round the correlation results and save them into B1
B1 <- round(B$r, 2)
# Save the p values into B2
B2 <- B$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(B1, file = "Results_corr_acq_SE_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(B2, file = "Results_corr_p_value_acq_SE_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Calculate the correlation matrix according Spearman
C <- rcorr(as.matrix(det_oe_a), type = "spearman")
# Round the correlation results and save them into C1
C1 <- round(C$r, 2)
# Save the p values into C2
C2 <- C$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(C1, file = "Results_corr_acq_SE_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(C2, file = "Results_corr_p_value_acq_SE_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

##########################################
# transformation into dataframe 
########################################## 

# before
data_oe_b <-  det_oe_b_alt 
# after
data_oe_a <- det_oe_a_alt 

##########################################
# linear regression 
########################################## 

##########################################
# SE (Oe) - acquisition before excl.
########################################## 

# leverage + leverage2
reg_oe_b <- lm(OE_b ~ leverage + leverage2, data = data_oe_b) 
summary(reg_oe_b)
# size + size2
reg_oe_b_I <- lm(OE_b ~ size + size2, data = data_oe_b) 
summary(reg_oe_b_I)
# leverage + size
reg_oe_b_II <- lm(OE_b ~ leverage + size, data = data_oe_b) 
summary(reg_oe_b_II)
# leverage + NDRS
reg_oe_b_III <- lm(OE_b ~ leverage + NDRS, data = data_oe_b) 
summary(reg_oe_b_III)
# size + liquid
reg_oe_b_IV <- lm(OE_b ~ size + liquid, data = data_oe_b) 
summary(reg_oe_b_IV)
# NDRS + liquid
reg_oe_b_V <- lm(OE_b ~ NDRS + liquid, data = data_oe_b) 
summary(reg_oe_b_V)
# Output scale efficiency
stargazer(reg_oe_b, reg_oe_b_I, reg_oe_b_II, reg_oe_b_III, reg_oe_b_IV, reg_oe_b_V,  
          title = "Regression analysis")

##########################################
# SE (Oe) - acquisition after excl.
########################################## 

# leverage + leverage2
reg_oe_a <- lm(OE_a ~ leverage + leverage2, data = data_oe_a) 
summary(reg_oe_a)
# size + size2
reg_oe_a_I <- lm(OE_a ~ size + size2, data = data_oe_a) 
summary(reg_oe_a_I)
# leverage + size
reg_oe_a_II <- lm(OE_a ~ leverage + size, data = data_oe_a) 
summary(reg_oe_a_II)
# leverage + NDRS
reg_oe_a_III <- lm(OE_a ~ leverage + NDRS, data = data_oe_a) 
summary(reg_oe_a_III)
# size + liquid
reg_oe_a_IV <- lm(OE_a ~ size + liquid, data = data_oe_a) 
summary(reg_oe_a_IV)
# NDRS + liquid
reg_oe_a_V <- lm(OE_a ~ NDRS + liquid, data = data_oe_a) 
summary(reg_oe_a_V)
# Output scale efficiency
stargazer(reg_oe_a, reg_oe_a_I, reg_oe_a_II, reg_oe_a_III, reg_oe_a_IV, reg_oe_a_V,  
          title = "Regression analysis")