getwd()
setwd("C:/Users/anupa/Desktop/ExcelR/Assignment/'09_Principal Component Analysis'/Universities_Clustering.csv")
#Reading the input file data
Univ_data1 <- read.csv(file.choose())
View(Univ_data1)
#calculating the Principle component Analysis
pca_objs <- princomp(Univ_data1[,-1], scores = TRUE, cor = TRUE, covmat = NULL)
summary(pca_objs)
loadings(pca_objs)
View(pca_objs)
#Get the scores for the Principle components generated
pca_objs$scores  

# As per the analysis , i want to consider the first 3 principle components which has around 94% of data.
#which will be suffiecent to perform clustering.
pca_objs$scores[,1:3]

# Combining the principle components to the actual data
univ_data2 <- cbind(Univ_data1,pca_objs$scores[,1:3])
View(univ_data2)

#Preparing the data for cluster analysis on whih it has 3 principle components.
clust_pca_data <- univ_data2[,8:10]
View(clust_pca_data)

#Calculating the distance b/w datapoints
dist1 <-  dist(clust_pca_data,method = "euclidean")
dist1

# Generating the clusters.
fit1 <- hclust(dist1,method = "complete")
plot(fit1,hang = -1)

#cutting the dendrogram for 5 clusters.
group_creation <- cutree(fit1,5)
View(group_creation)

# Assigning the each row for its respective cluster.
Univ_data1 <- cbind(group_creation,Univ_data1)
View(Univ_data1)

# Do the analysis with aggregated results.
Agg_avg <- aggregate(Univ_data1[,-2],by=list(group_creation),FUN = mean)
View(Agg_avg)



