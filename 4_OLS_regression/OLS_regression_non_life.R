##########################################
# OLS regression (non-life)
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
# NON-Life
# before excl. import
##########################################

# TE
# CRS data efficiency
# Path of the files
Path <- "C:/xxxx"

# PTE
# VRS data efficiency
# Path of the files
# Path <- "C:/xxxx"

# Control matrix forr non-acquirer
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
# Efficiency scores:
OE_b <- file_contents[[1]]
PE_b <- file_contents[[2]]
IE_b <- file_contents[[3]]

##########################################
# NON-Life
# after excl. import
##########################################

# TE
# CRS data efficiency
# Path of the files
 Path <- "C:/xxxx"

# VRS data efficiency
# Path of the files
# Path <- "C:/xxxx"

# Control matrix forr non-acquirer
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
# Efificiency scores:
OE_a <- file_contents[[1]]
PE_a <- file_contents[[2]]
IE_a <- file_contents[[3]]

##########################################
# data frames
##########################################

# before excl.:
data_oe_b <- data.frame(OE_b, det_b)
data_pe_b <- data.frame(PE_b, det_b)
data_ie_b <- data.frame(IE_b, det_b)
# after excl.:
data_oe_a <- data.frame(OE_a, det_a)
data_pe_a <- data.frame(PE_a, det_a)
data_ie_a <- data.frame(IE_a, det_a)

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

# data_pe_b
PE_b <- t(PE_b)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(PE_b) - tf*mad(PE_b)
upper_bound <- median(PE_b) + tf*mad(PE_b)
# Place of the outliers
outlier_ind <- which(PE_b < lower_bound | PE_b > upper_bound)
# delete rows
data_pe_b <- data_pe_b[-outlier_ind,]

# data_ie_b
IE_b <- t(IE_b)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(IE_b) - tf*mad(IE_b)
upper_bound <- median(IE_b) + tf*mad(IE_b)
# Place of the outliers
outlier_ind <- which(IE_b < lower_bound | IE_b > upper_bound)
# delete rows
data_ie_b <- data_ie_b[-outlier_ind,]

# data_oe_a
OE_a <- t(OE_a)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(OE_a) - tf*mad(OE_a)
upper_bound <- median(OE_a) + tf*mad(OE_a)
# Place of the outliers
outlier_ind <- which(OE_a < lower_bound | OE_a > upper_bound)
# delete rows
data_oe_a <- data_oe_a[-outlier_ind,]

# data_pe_a
PE_a <- t(PE_a)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(PE_a) - tf*mad(PE_a)
upper_bound <- median(PE_a) + tf*mad(PE_a)
# Place of the outliers
outlier_ind <- which(PE_a < lower_bound | PE_a > upper_bound)
# delete rows
data_pe_a <- data_pe_a[-outlier_ind,]

# data_ie_a
IE_a <- t(IE_a)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(IE_a) - tf*mad(IE_a)
upper_bound <- median(IE_a) + tf*mad(IE_a)
# Place of the outliers
outlier_ind <- which(IE_a < lower_bound | IE_a > upper_bound)
# delete rows
data_ie_a <- data_ie_a[-outlier_ind,]

##########################################
# Transformation dataframe
##########################################

# before
det_oe_b_alt <- data_oe_b
det_pe_b_alt <- data_pe_b
det_ie_b_alt <- data_ie_b
# after
det_oe_a_alt <- data_oe_a
det_pe_a_alt <- data_pe_a
det_ie_a_alt <- data_ie_a
# before
det_oe_b <- data_oe_b[,-1]
det_pe_b <- data_pe_b[,-1]
det_ie_b <- data_ie_b[,-1]
# after
det_oe_a <- data_oe_a[,-1] 
det_pe_a <- data_pe_a[,-1]
det_ie_a <- data_ie_a[,-1]

##########################################
# Check normal distribution:
##########################################

# acquisition before excl.
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 1)
res_p <- matrix(0, 6, 1)
# perform a for loop
for(i in 1:6){
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_oe_b[,i])
  # Save d statistics into res
  res[i,1] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_SW_acq_oe_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_SW_p_value_acq_oe_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl. 6
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 1)
res_p <- matrix(0, 6, 1)
# perform a for loop
for(i in 1:6){
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_oe_a[,i])
  # Save d statistics into res
  res[i,1] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_SW_acq_oe_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_SW_p_value_acq_oe_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# production efficiency
# acquisition before excl. 
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 1)
res_p <- matrix(0, 6, 1)
# perform a for loop
for(i in 1:6){
  # Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_pe_b[,i])
  # Save d statistics into res
  res[i,1] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_SW_acq_pe_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_SW_p_value_acq_pe_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 1)
res_p <- matrix(0, 6, 1)
# perform a for loop
for(i in 1:6){
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_pe_a[,i])
  # Save d statistics into res
  res[i,1] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_SW_acq_pe_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_SW_p_value_acq_pe_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# Investment efficiency
# acquisition before excl.
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 1)
res_p <- matrix(0, 6, 1)
# perform a for loop
for(i in 1:6){
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_ie_b[,i])
  # Save d statistics into res
  res[i,1] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_SW_acq_ie_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_SW_p_value_acq_ie_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Initialization of the result matrix res and res_2
res <- matrix(0, 6, 1)
res_p <- matrix(0, 6, 1)
# perform a for loop
for(i in 1:6){
  #Check normal distribution with the Shapiro-Wilk test and save it into res
  SW <- shapiro.test(det_ie_a[,i])
  # Save d statistics into res
  res[i,1] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_SW_acq_ie_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(res_p, file = "Results_SW_p_value_acq_ie_a.csv", row.names = FALSE, 
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
write.table(B1, file = "Results_corr_acq_oe_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(B2, file = "Results_corr_p_value_acq_oe_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Calculate the correlation matrix according Spearman
C <- rcorr(as.matrix(det_oe_a), type = "spearman")
# Round the correlation results and save them into C1
C1 <- round(C$r, 2)
# Save the p values into C2
C2 <- C$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(C1, file = "Results_corr_acq_oe_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(C2, file = "Results_corr_p_value_acq_oe_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# Production efficiency
# acquisition before excl.
# Calculate the correlation matrix according Spearman
B <- rcorr(as.matrix(det_pe_b), type = "spearman")
# Round the correlation results and save them into B1
B1 <- round(B$r, 2)
# Save the p values into B2
B2 <- B$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(B1, file = "Results_corr_acq_pe_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(B2, file = "Results_corr_p_value_acq_pe_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Calculate the correlation matrix according Spearman
C <- rcorr(as.matrix(det_pe_a), type = "spearman")
# Round the correlation results and save them into C1
C1 <- round(C$r, 2)
# Save the p values into C2
C2 <- C$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(C1, file = "Results_corr_acq_pe_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(C2, file = "Results_corr_p_value_acq_pe_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# Investment efficiency
# acquisition before excl.
# Calculate the correlation matrix according Spearman
B <- rcorr(as.matrix(det_ie_b), type = "spearman")
# Round the correlation results and save them into B1
B1 <- round(B$r, 2)
# Save the p values into B2
B2 <- B$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(B1, file = "Results_corr_acq_ie_b.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(B2, file = "Results_corr_p_value_acq_ie_b.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

# acquisition after excl.
# Calculate the correlation matrix according Spearman
C <- rcorr(as.matrix(det_ie_a), type = "spearman")
# Round the correlation results and save them into C1
C1 <- round(C$r, 2)
# Save the p values into C2
C2 <- C$P
# Outputs the correlation matrix B1 and the p values which where saved into B2
write.table(C1, file = "Results_corr_acq_ie_a.csv", row.names = FALSE, dec = ".", 
            sep = ";", quote = FALSE)
write.table(C2, file = "Results_corr_p_value_acq_ie_a.csv", row.names = FALSE, 
            dec = ".", sep = ";", quote = FALSE)

##########################################
# transformation into dataframe
########################################## 

# before
data_oe_b <-  det_oe_b_alt 
data_pe_b <-  det_pe_b_alt 
data_ie_b <- det_ie_b_alt 
# after
data_oe_a <- det_oe_a_alt 
data_pe_a <- det_pe_a_alt 
data_ie_a <- det_ie_a_alt 

##########################################
# linear regression 
########################################## 

##########################################
# TE (Oe) - acquisition before excl.
########################################## 

# OE: leverage + leverage2
reg_oe_b_nl <- lm(OE_b ~ leverage + leverage2, data = data_oe_b)
summary(reg_oe_b_nl)
# OE: size + size2
reg_oe_b_nl_I <- lm(OE_b ~ size + size2, data = data_oe_b)
summary(reg_oe_b_nl_I)
# OE: leverage + size
reg_oe_b_nl_II <- lm(OE_b ~ leverage + size, data = data_oe_b)
summary(reg_oe_b_nl_II)
# OE: leverage + ndrs
reg_oe_b_nl_III <- lm(OE_b ~ leverage + NDRS, data = data_oe_b)
summary(reg_oe_b_nl_III)
# OE: size + ndrs
reg_oe_b_nl_IV <- lm(OE_b ~ size + NDRS, data = data_oe_b)
summary(reg_oe_b_nl_IV)
# OE: leverage + liquid
reg_oe_b_nl_V <- lm(OE_b ~ leverage + liquid, data = data_oe_b)
summary(reg_oe_b_nl_V)
# OE: ndrs + liquid
reg_oe_b_nl_VI <- lm(OE_b ~ NDRS + liquid, data = data_oe_b)
summary(reg_oe_b_nl_VI)
# Output latex table with the results 
stargazer(reg_oe_b_nl, reg_oe_b_nl_I, reg_oe_b_nl_II, reg_oe_b_nl_III, reg_oe_b_nl_IV, reg_oe_b_nl_V, 
          reg_oe_b_nl_VI,  title = "Regression analysis")

##########################################
# TE (Oe) - acquisition after excl.
########################################## 

# OE: leverage + leverage2
reg_oe_a_nl <- lm(OE_a ~ leverage + leverage2, data = data_oe_a)
summary(reg_oe_a_nl)
# OE: size + size2
reg_oe_a_nl_I <- lm(OE_a ~ size + size2, data = data_oe_a)
summary(reg_oe_a_nl_I)
# OE: leverage + size
reg_oe_a_nl_II <- lm(OE_a ~ leverage + size, data = data_oe_a)
summary(reg_oe_a_nl_II)
# OE: leverage + ndrs
reg_oe_a_nl_III <- lm(OE_a ~ leverage + ndrs, data = data_oe_a)
summary(reg_oe_a_nl_III)
# OE: size + ndrs
reg_oe_a_nl_IV <- lm(OE_a ~ size + ndrs, data = data_oe_a)
summary(reg_oe_a_nl_IV)
# OE: leverage + liquid
reg_oe_a_nl_V <- lm(OE_a ~ leverage + liquid, data = data_oe_a)
summary(reg_oe_a_nl_V)
# OE: size + liquid
reg_oe_a_nl_VI <- lm(OE_a ~ size + liquid, data = data_oe_a)
summary(reg_oe_a_nl_VI)
# OE: ndrs + liquid
reg_oe_a_nl_VII <- lm(OE_a ~ ndrs + liquid, data = data_oe_a)
summary(reg_oe_a_nl_VII)
# Output latex table with the results 
stargazer(reg_oe_a_nl, reg_oe_a_nl_I, reg_oe_a_nl_II, reg_oe_a_nl_III, reg_oe_a_nl_IV, reg_oe_a_nl_V, 
          reg_oe_a_nl_VI, reg_oe_a_nl_VII,  title = "Regression analysis")

##########################################
# PTE (Oe) - acquisition before excl.
########################################## 

# OE: leverage + leverage2
reg_oe_b_nl <- lm(OE_b ~ leverage + leverage2, data = data_oe_b)
summary(reg_oe_b_nl)
# OE: size + size2
reg_oe_b_nl_I <- lm(OE_b ~ size + size2, data = data_oe_b)
summary(reg_oe_b_nl_I)
# OE: leverage + size
reg_oe_b_nl_II <- lm(OE_b ~ leverage + size, data = data_oe_b)
summary(reg_oe_b_nl_II)
# OE: leverage + ndrs
reg_oe_b_nl_III <- lm(OE_b ~ leverage + NDRS, data = data_oe_b)
summary(reg_oe_b_nl_III)
# OE: size + ndrs
reg_oe_b_nl_IV <- lm(OE_b ~ size + NDRS, data = data_oe_b)
summary(reg_oe_b_nl_IV)
# OE: ndrs + liquid
reg_oe_b_nl_V <- lm(OE_b ~ NDRS + liquid, data = data_oe_b)
summary(reg_oe_b_nl_V)
# Output latex table with the results 
stargazer(reg_oe_b_nl, reg_oe_b_nl_I, reg_oe_b_nl_II, reg_oe_b_nl_III, reg_oe_b_nl_IV, reg_oe_b_nl_V,  
          title = "Regression analysis")

##########################################
# TE (Pe) - acquisition before excl.
##########################################

# PE: leverage + leverage2
reg_pe_b_nl <- lm(PE_b ~ leverage + leverage2, data = data_pe_b)
summary(reg_pe_b_nl)
# PE: size + size2
reg_pe_b_nl_I <- lm(PE_b ~ size + size2, data = data_pe_b)
summary(reg_pe_b_nl_I)
# PE: leverage + size
reg_pe_b_nl_II <- lm(PE_b ~ leverage + size, data = data_pe_b)
summary(reg_pe_b_nl_II)
# PE: leverage + NDRS
reg_pe_b_nl_III <- lm(PE_b ~ leverage + NDRS, data = data_pe_b)
summary(reg_pe_b_nl_III)
# PE: size + NDRS
reg_pe_b_nl_IV <- lm(PE_b ~ size + NDRS, data = data_pe_b)
summary(reg_pe_b_nl_IV)
# PE: ndrs + liquid
reg_pe_b_nl_V <- lm(PE_b ~ NDRS + liquid, data = data_pe_b)
summary(reg_pe_b_nl_V)
# Output latex table with the results 
stargazer(reg_pe_b_nl, reg_pe_b_nl_I, reg_pe_b_nl_II, reg_pe_b_nl_III, reg_pe_b_nl_IV, reg_pe_b_nl_V,  
          title = "Regression analysis")

##########################################
# TE (Pe) - acquisition after excl.
##########################################

# PE: leverage + size
reg_pe_a_nl <- lm(PE_a ~ leverage + leverage2, data = data_pe_a)
summary(reg_pe_a_nl)
# PE: size + size2
reg_pe_a_nl_I <- lm(PE_a ~ size + size2, data = data_pe_a)
summary(reg_pe_a_nl_I)
# PE: leverage + size
reg_pe_a_nl_II <- lm(PE_a ~ leverage + size, data = data_pe_a)
summary(reg_pe_a_nl_II)
# PE: size + ndrs
reg_pe_a_nl_III <- lm(PE_a ~ size + ndrs, data = data_pe_a)
summary(reg_pe_a_nl_III)
# PE: leverage + liquid
reg_pe_a_nl_IV <- lm(PE_a ~ leverage + liquid, data = data_pe_a)
summary(reg_pe_a_nl_IV)
# PE: size + liquid
reg_pe_a_nl_V <- lm(PE_a ~ size + liquid, data = data_pe_a)
summary(reg_pe_a_nl_V)
# PE:  ndrs + liquid
reg_pe_a_nl_VI <- lm(PE_a ~ ndrs + liquid, data = data_pe_a)
summary(reg_pe_a_nl_VI)
# Output latex table with the results 
stargazer(reg_pe_a_nl, reg_pe_a_nl_I, reg_pe_a_nl_II, reg_pe_a_nl_III, reg_pe_a_nl_IV, reg_pe_a_nl_V, 
          reg_pe_a_nl_VI, title = "Regression analysis")

##########################################
# Cheking OLS assumption 
# TE (Oe) - acquisition before excl.
########################################## 

# Checking assumption model VI
# Check of normality of the residuals
shapiro.test(reg_oe_b_nl_V$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_oe_b_nl_V)

##########################################
# Cheking OLS assumption 
# TE (Oe) - acquisition after excl.
########################################## 

# Checking assumption model IV
# Check of normality of the residuals
shapiro.test(reg_oe_a_nl_IV$residuals)

# Breusch-Pegang test (Homoscedasticity)
bptest(reg_oe_a_nl_IV)

# Checking assumption model V
# Check of normality of the residuals
shapiro.test(reg_oe_a_nl_V$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_oe_a_nl_V)

# Checking assumption model VI
# Check of normality of the residuals
shapiro.test(reg_oe_a_nl_VI$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_oe_a_nl_VI)
# adjustment of standard errors
coeftest(reg_oe_a_nl_VI, vcov=vcovHC(reg_oe_a_nl_VI, type=c("HC3")))

# Checking assumption model VII
# Check of normality of the residuals
shapiro.test(reg_oe_a_nl_VII$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_oe_a_nl_VII)

##########################################
# Cheking OLS assumption 
# PTE (Oe) - acquisition before excl.
########################################## 

# Checking assumption model VI
# Check of normality of the residuals
shapiro.test(reg_oe_b_nl_V$residuals)
# Breusch-pegan test (Homoscedasticity)
bptest(reg_oe_b_nl_V)
# adjustment of standard errors
coeftest(reg_oe_b_nl_V, vcov=vcovHC(reg_oe_b_nl_V, type=c("HC3")))

##########################################
# Cheking OLS assumption 
# TE (Pe) - acquisition after excl.
##########################################

# Checking assumption model I
# Check of normality of the residuals
shapiro.test(reg_pe_a_nl_I$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_pe_a_nl_I)

# Checking assumption model III
# Check of normality of the residuals
shapiro.test(reg_pe_a_nl_III$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_pe_a_nl_III)

# Checking assumption model VI
# Check of normality of the residuals
shapiro.test(reg_pe_a_nl_VI$residuals)
# Breusch-Pegang test (Homoscedasticity)
bptest(reg_pe_a_nl_VI)