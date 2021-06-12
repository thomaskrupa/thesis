###########################################
# (3) Relevance of the variables
###########################################

### This R code checks the relevance of the variables in the NSBM approach. The investigation 
### is running separately for the life and non-life sector. Log-log regression is performed 
### for the first and second stage as well as the robustness check. First, the two outputs 
### investment income and profit are transformed so that they only contain positive values. 
### Then the regression models are run with the R function lm(). The results are then imported 
### into Latex using the R function stargazer.
library(stargazer) # R to latex

###########################################
# Log- Log Regression analysis Life
###########################################

# Path of the input file (life)
Path <- "xxxx.csv"

# Read the data inside a dataframe df
df_1 <- read.csv(file = Path, header=TRUE, sep = ";");
df <- df_1
df <- df[,2:9]

# first division
reg_L_L1 <- lm(log(df[,4])~log(df[,1])+log(df[,2])+log(df[,3]))
reg_L_L2 <- lm(log(df[,5])~log(df[,1])+log(df[,2])+log(df[,3]))

# second division
# transform the variable investment income to positive
income_adj <- df$Investment.income - min(df$Investment.income) + 1 ;
reg_L_O1 <- lm(log(income_adj)~log(df[,4])+log(df[,5]))

# Robustness check
reg_L_RC1 <- lm(log(df[,8])~log(df[,1])+log(df[,2])+log(df[,3]))
#reg_L_RC2 <- lm(log(df[,9])~log(df[,1])+log(df[,2])+log(df[,3]))
reg_L_RC1_1 <- lm(log(income_adj)~log(df[,8])+log(df[,5]))

# Output latex table with the results
stargazer(reg_L_L1, reg_L_L2, reg_L_O1, reg_L_RC1, reg_L_RC1_1, title = "Regression analysis")

# Problems with coefficient
# transform the variable profit to positive
profits_adj <- df$Profits - min(df$Profits) + 1 ;
reg_L_O2 <- lm(log(profits_adj)~log(df[,4])+log(df[,5]))

###########################################
# Log- Log Regression analysis Non-life
###########################################

# Path of the input file (non-life)
Path <- "xxxx.csv"

# Read the data transform into a dataframe df
df_1 <- read.csv(file = Path, header=TRUE, sep = ";");
df <- df_1
df <- df[,2:9]

# first division
reg_NL_L1 <- lm(log(df[,4])~log(df[,1])+log(df[,2])+log(df[,3]))
reg_NL_L2 <- lm(log(df[,5])~log(df[,1])+log(df[,2])+log(df[,3]))

# second division
# transform the variables investment income and profits to positive
income_adj <- df$Investment.income - min(df$Investment.income) + 1 ;
profits_adj <- df$Profits - min(df$Profits) +1;
reg_NL_O1 <- lm(log(income_adj)~log(df[,5]))
reg_NL_O2 <- lm(log(profits_adj)~log(df[,5]))

# Robustness check
reg_NL_RC1 <- lm(log(df[,8])~log(df[,1])+log(df[,2])+log(df[,3]))

# Output latex table with the results
stargazer(reg_NL_L1, reg_NL_L2, reg_NL_O1, reg_NL_O2, reg_NL_RC1, title = "Regression analysis")

# Problems with coefficient
reg_NL_O1_1 <- lm(log(income_adj)~log(df[,4])+log(df[,5]))
reg_NL_O2_1 <- lm(log(profits_adj)~log(df[,4])+log(df[,5]))