###########################################
# Statistical methods (life)
###########################################

library(tidyverse)
library(fs)
library(psych)
library(ggplot2)
library(patchwork)
library(Routliers)
library (car) # Levene test
library(ggpubr)
library(MKinfer) # bootstrapping t and welch test
library(fBasics) # omnibus agostino and jarque bera test
library(RVAideMemoire) # median test

### This code perform the statistical methods for the life insurance sector. First paths should be adjusted.
### The first path save the place where the results should be saved. Then, one should decided which efficiency
### types is analysed and adjust the path. The statistical methods contains graphical methods, normality
### tests, homogeneity tests and two-sample tests.Which two-sample tests should be used should be assessed 
### against the previous criteria. In the two-sample tests, only one-sided tests are used. For this reason, 
### a decision must be made between the greater or less option. This decision can be made by looking at the 
### means or medians of the two samples to be tested.

# Save results in this path
setwd("C:/xxxx")

##########################################
# Paths
# CRS -> TE
##########################################

# Overall efficiency
 Path <- "C:/xxxx"

# Production efficiency
 # Path <- "C:/xxxx"

# Investment efficiency
# Path <- "C:/xxxx"

##########################################
# Paths
# VRS -> PTE
##########################################

# Overall efficiency
# Path <- "C:/xxxx"

##########################################
# Paths
# SE <- Scale
##########################################

# Scale efficiency
# Path <- "C:/xxxx"

###########################################
# Import data and transformation
###########################################

file_paths <- fs::dir_ls(Path)
# For loop: read files
file_contents <- list();
for(i in seq_along(file_paths)){
  # read the files of the file_paths and save them into file_contents
  file_contents[[i]] <- read.csv(file = file_paths[[i]], header=TRUE, sep = ";");
}
# Rename the file names in the list file_contents with the paths in file_paths:
file_contents <- set_names(file_contents, file_paths)
# Transform into a datafram:
dfx <- data.frame(file_contents[[1]])
dfy <- data.frame(file_contents[[2]])
dfz <- data.frame(file_contents[[3]])
dfx <- t(dfx)
dfy <- t(dfy)
dfz <- t(dfz)

###########################################
# Pre-Testing
###########################################

# threefold parameter
tf= 2.0
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(dfx) - tf*mad(dfx)
upper_bound <- median(dfx) + tf*mad(dfx)
# Place of the outliers
outlier_ind <- which(dfx < lower_bound | dfx > upper_bound)
print(dim(t(outlier_ind))[2])
dfx2 <- dfx[-c(outlier_ind)]
dfx2 <- t(dfx2)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(dfy) - tf*mad(dfy)
upper_bound <- median(dfy) + tf*mad(dfy)
# Place of the outliers
outlier_ind_1 <- which(dfy < lower_bound | dfy > upper_bound)
print(dim(t(outlier_ind_1))[2])
dfy2 <- dfy[-c(outlier_ind_1)]
dfy2 <- t(dfy2)
# Calculates lower and upper bound Humpel filter:
lower_bound <- median(dfz) - tf*mad(dfz)
upper_bound <- median(dfz) + tf*mad(dfz)
# Place of the outliers
outlier_ind_2 <- which(dfz < lower_bound | dfz > upper_bound)
print(dim(t(outlier_ind_2))[2])
dfz2 <- dfz[-c(outlier_ind_2)]
dfz2 <- t(dfz2)

###########################################
# Transformation dataframe
###########################################

dfx2 <- t(dfx2)
dfx2 <- data.frame(dfx2)
dfy2 <- t(dfy2)
dfy2 <- data.frame(dfy2)
dfz2 <- t(dfz2)
dfz2 <- data.frame(dfz2)

###########################################
# Graphical methods
###########################################

# Histrograms
h1 <- ggplot(dfx2, aes(x = dfx2)) +
  geom_histogram( color = "grey30", fill = "white") +
  ggtitle("Non-acquirer")+
  labs(x ="Ratio", y="count")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))

h2 <- ggplot(dfy2, aes(x = dfy2)) +
  geom_histogram(color = "grey30", fill = "white", bins = 8) +
  ggtitle("Acquirer before excl.")+
  labs(x ="Ratio", y="count")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))

h3 <- ggplot(dfz2, aes(x = dfz2)) +
  geom_histogram(color = "grey30", fill = "white", bins = 5) +
  ggtitle("Acquirer after excl.") +
  labs(x ="Ratio", y="count")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))

# Distribution of the samples
sc1 <- ggdensity(dfx2, x = "dfx2", fill = "lightgray") +
  scale_x_continuous(limits = c(-0.5, 2.5)) +
  labs(x ="", y="density")+
  stat_overlay_normal_density(color = "red", linetype = "dashed")

sc2 <- ggdensity(dfy2, x = "dfy2", fill = "lightgray") +
  scale_x_continuous(limits = c(-0.5, 2.5)) +
  labs(x ="", y="density")+
  stat_overlay_normal_density(color = "red", linetype = "dashed")


sc3 <- ggdensity(dfz2, x = "dfz2", fill = "lightgray") +
  scale_x_continuous(limits = c(-0.5, 2.5)) +
  labs(x ="", y="density")+
  stat_overlay_normal_density(color = "red", linetype = "dashed")


###########################################
# Transformation dataframe
###########################################

a <- 1
dfx2 <- data.frame(dfx2,a)
b <- 2
dfy2 <- data.frame(dfy2, b)
c <- 3
dfz2 <- data.frame(dfz2, c)

# Boxplot
b1 <- ggplot(dfx2, aes(y = dfx2, x = a)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(fill = "white", outlier.size = 2, outlier.shape = 8, outlier.color = "red") +
  labs(x ="", y="Ratio")+
  scale_x_discrete()+
  scale_y_continuous(limits=c(0.25,1.75), breaks=seq(0.25,1.75,0.25))+
  ggtitle("Non-acquirer")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

b2 <- ggplot(dfy2, aes(y = dfy2, x = b)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(fill = "white", outlier.size = 2, outlier.shape = 8, outlier.color = "red") +
  labs(x ="", y="Ratio")+
  scale_x_discrete()+
  scale_y_continuous(limits=c(0.25,1.75), breaks=seq(0.25,1.75,0.25))+
  ggtitle("Acquirer before excl.")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

b3 <- ggplot(dfz2, aes(y = dfz2, x = c)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(fill = "white", outlier.size = 2, outlier.shape = 8, outlier.color = "red") +
  labs(x ="", y="Ratio")+
  scale_x_discrete()+
  scale_y_continuous(limits=c(0.25,1.75), breaks=seq(0.25,1.75,0.25))+
  ggtitle("Acquirer after excl.") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

 # Q Q plot
 g1 <- ggplot(dfx2, aes(sample = dfx2))+
       stat_qq() + stat_qq_line()

 g2 <- ggplot(dfy2, aes(sample = dfy2))+
       stat_qq() + stat_qq_line()

 g3 <- ggplot(dfz2, aes(sample = dfz2))+
       stat_qq() + stat_qq_line()

# Output of the statistics
(h1 + h2 + h3) /
(sc1 + sc2 + sc3)/
(g1 + g2 + g3)
# Output boxplots
 b1 + b2 + b3

# Sample size
 print(dim(dfx2)[1])
 print(dim(dfy2)[1])
 print(dim(dfz2)[1])

 ##############################################
 # Test for normality
 ##############################################

 # Non-acquirer sample
 shapiro.test(dfx2$dfx2)
 dim(dfx2)[1]
 
 # Acquirer before excl. sample
 shapiro.test(dfy2$dfy2)
 dim(dfy2)[1]
 
 # Acquirer after excl. sample
 shapiro.test(dfz2$dfz2)
 dim(dfz2)[1]

describeBy(dfx2)
describeBy(dfy2)
describeBy(dfz2)

 ##############################################
 # Test for variance homogeneity
 ##############################################

 # test variance homogeneity
 colnames(dfx2) <- c("Ratio", "AC")
 colnames(dfz2) <- c("Ratio", "AC")
 colnames(dfy2) <- c("Ratio", "AC")

 k <- rbind(dfx2, dfy2)
 l <- rbind(dfx2, dfz2)
 d <- rbind(dfx2, dfy2, dfz2)

# non-acquirer vs acquirer before samples
leveneTest(k$Ratio, k$AC)

# non-acquirer vs acquirer after samples
leveneTest(l$Ratio, l$AC)

 ######################################################
 # Parametric test & Non-parametric two-sample tests
 ######################################################

 # option is either greater or less dependent on the mean and should be adjusted maybe
 # compare means
 describeBy(Ratio~AC, data = k)
 #  non-acquirer vs acquirer before excl.samples
 
 # t test
 t.test(Ratio~AC, data=k, var.equal = TRUE, alternative = "greater")
 # T test bootstrap
 set.seed(1315)
 boot.t.test(dfx2$Ratio, dfy2$Ratio, alternative ="greater", paired = FALSE, mu = 0, var.equal = TRUE,
            conf.level = 0.95, R = 10000)
 # Welch test
 t.test(Ratio~AC, data=k, var.equal = FALSE,  alternative ="greater")
 # Welch test bootstrap
 set.seed(1315)
 boot.t.test(dfx2$Ratio, dfy2$Ratio, alternative ="greater", paired = FALSE, mu = 0, var.equal = FALSE,
             conf.level = 0.95, R = 10000)
 # Mann-Whitney U test
 wilcox.test(Ratio~AC, data=k, exact=FALSE, alternative ="greater")

 #  non-acquirer vs acquirer after excl. samples
 # compare means
 describeBy(Ratio~AC, data = l)
 
 # t test
 t.test(Ratio~AC, data=l, var.equal = TRUE, alternative = "greater")
 # T test bootstrap
 set.seed(1315)
 boot.t.test(dfx2$Ratio, dfz2$Ratio, alternative ="greater", paired = FALSE, mu = 0, var.equal = TRUE,
             conf.level = 0.95, R = 10000)
 # Welch test
 t.test(Ratio~AC, data=l, var.equal = FALSE, alternative ="greater")
 # Welch test bootstrap
 set.seed(1315)
 boot.t.test(dfx2$Ratio, dfz2$Ratio, alternative ="less", paired = FALSE, mu = 0, var.equal = FALSE,
             conf.level = 0.95, R = 10000)
 # Mann-Whitney U test
 wilcox.test(Ratio~AC, data=l, exact=FALSE, alternative ="greater")