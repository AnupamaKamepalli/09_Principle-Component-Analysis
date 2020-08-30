getwd()
#Reading the input file.
wine_d1 <- read.csv(file.choose())
View(wine_d1)

#Preparing the data for analysis purpose.
wine_d2 <- wine_d1[,-1]
View(wine_d2)

#Principle component analysis.
wine_d2.pca <- princomp(wine_d2,scores = TRUE,cor = TRUE,covmat = NULL)
loadings(wine_d2.pca)
summary(wine_d2.pca)
# As per the summary, PC1 to PC7 contains of approx 90% of information. so which will be sufficient for further analysis.

#To check the scores of principle components.
wine_d2.pca$scores
wine_d2.pca_Final <- wine_d2.pca$scores[,1:7]
View(wine_d2.pca_Final)

# Clustering Analysis on Actual data without Principle components

install.packages("NbClust")
library(NbClust)
Cluster_Numb1 <- NbClust(wine_d2,method = "complete",distance = "euclidean")
# I see from the plots, there will be minimal diffrence from 5 to 11 clusters. Inorder to analyze more, i have written the below code.
Cluster_Numb2 <- NbClust(wine_d2,method = "complete",distance = "euclidean",min.nc = 2,max.nc = 10) 
Cluster_Numb2$Best.partition # To see cluster number for each record.
# As per the results we conclude that the best number of clusters is '7'.

#Plot the data ,to find the optimum number of clusters. 
install.packages("factoextra")
library(factoextra)
fviz_nbclust(Cluster_Numb2)

#K- Means clustering on actual data without principle components.
wine_d2_norm <- scale(wine_d2)
fviz_nbclust(wine_d2_norm,kmeans,method = "wss") # The optimun number of cluster could be 7 as per the elbow curve.

fit_kmns <- kmeans(wine_d2_norm,7)
fit_kmns$cluster

# Clustering Analysis on Actual data with Principle components
clust_num_pc1 <- NbClust(wine_d2.pca_Final,method = "complete",distance = "euclidean")
clust_num_pc2 <- NbClust(wine_d2.pca_Final,method = "complete",distance = "euclidean",min.nc = 2,max.nc = 10) 

# As per the data, The optimum number of clusters could be '4'.
clust_num_pc2$Best.partition
wine_final <- cbind(clust_num_pc2$Best.partition,wine_d1)
View(wine_final)

#Note : Due to dimension reduction we can also reduce the no. of clusters.

