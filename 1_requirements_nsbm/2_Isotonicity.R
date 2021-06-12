###########################################
# (2) Isotonicity
###########################################

### The Isotonicity assumption is checked. First, normality is tested by Kolmogorov-Smirnov and 
### Shapiro-Wilk test. Then, the Spearman rho corrleation is performed and the results are outputed.
library("Hmisc") # Using rcorr

###########################################
# Import data and transform into numeric
###########################################

# Path of input file
Path <- "xxxx.csv"

# Read the data and transform into dataframe
df_1 <- read.csv(file = Path, header=TRUE, sep = ";");
df <- df_1
df <- df[,2:10]

###########################################
# Check normal distribution:
###########################################

# Initialization of the result matrix res and res_2
res <- matrix(0, 9, 2)
res_p <- matrix(0, 9, 2)
# perform a for loop
for(i in 1:9){
  # Check normal distribution with the Kolmogorov-Smirnov test and save it into KS
  KS <- ks.test(df[,i],"pnorm", mean(df[,i]), sd(df[,i]))
  #Check normal distribution with the Shapiro-Wilk test and save it into SW
  SW <- shapiro.test(df[,i])
  # Save d statistics into res
  res[i,1] <- round(KS$statistic,5)
  res[i,2] <- round(SW$statistic,5)
  # Save p values into res_2
  res_p[i,1] <- KS$p.value
  res_p[i,2] <- SW$p.value
}
# Outputs the results of KS test and SW test 
write.table(res, file = "Results_KS_SW.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(res_p, file = "Results_KS_SW_p_value.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)

###########################################
# Spearman rho correlation
###########################################

# Calculate the correlation matrix according Spearman
B <- rcorr(as.matrix(df), type = "spearman")
# Round the correlation results and save them into B1
B1 <- round(B$r, 2)
# Save the p values into B2
B2 <- B$P

# Outputs the correlation matrix (B1) and the p values (B2)
write.table(B1, file = "Results_corr.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
write.table(B2, file = "Results_corr_p_value.csv", row.names = FALSE, dec = ".", sep = ";", quote = FALSE)