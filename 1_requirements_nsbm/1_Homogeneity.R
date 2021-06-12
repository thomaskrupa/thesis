############################################
# (1) Homogeneity
###########################################

### R code to check the homogeneity assumption with the Ward method. Data are scaled and euclidian distance 
### is calculated.Then, Ward method is applied and illustrated in a dendogram.
library(psych)       # Clustering algorithms
library(factoextra)  # Clustering visualization
library(NbClust)     # Model Validation

##########################################
# Import data and transformation
##########################################

# Path of input file
Path <- "xxxx.csv"

# Read the data and transform into dataframe
ac <- read.csv(file = Path, header=TRUE, sep = ";");
ac_2 <- ac[,2:71]
# Scale the vector
ac.scaled <- scale(ac_2)

###########################################
# Ward method
###########################################

# Calculate distance with the euclidean distance
dist.ac <- dist(ac.scaled , method = "euclidean");
# Applying the cluster analysis with the Ward method
fit.ac <- hclust(dist.ac , method = "ward.D2")
# plot dendrogram
plot(fit.ac, hang = -1, labels = ac$NR, cex =.7, main = "Cluster Dendrogram", xlab = "")

###########################################
# Determining Optimal Clusters
###########################################

set.seed(123)
# The method NbClust used 30 different methods to find the optimal cluster number
NbClust(data = ac_2 , diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 5, method = "ward.D2")

###########################################
# Plot
###########################################

# Final plot of the dendrogram with k clusters
fviz_dend(fit.ac, k = 3,             
          cex = 0.5,                 
          k_colors = c("black", "blue", "red"), 
          color_labels_by_k = TRUE,  
          main = "",                 
          ggtheme = theme_classic()  
)

###########################################
# Save the solution
###########################################

# k- Cluster-solution and combine results with the numbers NR
T <- cutree(fit.ac, k=3)
T1 <- cbind(ac$NR, T)
write.csv2(T1, "Ward_method_nl.csv")