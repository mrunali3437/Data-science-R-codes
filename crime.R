#Clustering for Crime Data

#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

#Lets Do Heirarchical Clustering
crime <- read.csv("C:/Users/abc/Desktop/Data science/Dataset/Clustering/crime_data.csv")
summary(crime)


#EDA
#Standard Deviation
sd(crime$Murder)

sd(crime$Assault)
sd(crime$UrbanPop)
sd(crime$Rape)


#Variance
var(crime$Murder)

var(crime$Assault)



var(crime$UrbanPop)
var(crime$Rape)


#correlation
cor(crime[,-1])


crime1<- scale(crime[,2:5]) 


d <-dist(crime1, method = "euclidean") 
d

options(max.print=999999) 
#Lets do it Using Centroid Linkage Method of hierarchical clustering
cluster1 = hclust(d, method = "centroid") 
plot(cluster1)  

tree1 <- cutree(cluster1,k= 4) 

tree1

rect.hclust(cluster1, k=4, border = 2:6) 
abline(h = 4, col = 'blue') 



groups <- data.frame("City"=crime[,1],"Cluster Number"=tree1)  

cluster2 <- hclust(d, method = "average") 
plot(cluster2)

tree2 <- cutree(cluster2, k=4)

rect.hclust(cluster2,k=4, border=2:6)

group2 <- data.frame("City"=crime[,1], "cluster number"= tree2)
group2


#Complete Linkage Method

cluster3 <- hclust(d, method = "complete")  
plot(cluster3)

tree3 <- cutree(cluster3, k=4)

rect.hclust(cluster3,k=4, border=2:6)

group3 <- data.frame("City"=crime[,1], "cluster number"= tree3)
group3

#Lets Use Single Linkage Method

cluster4 <- hclust(d, method = "single")  #minimum distance
plot(cluster4)

tree4 <- cutree(cluster4, k=5)

rect.hclust(cluster4,k=4, border=2:6)

group4 <- data.frame("City"=crime[,1], "cluster number"= tree4)

group4

#Lets Do the Clustering With KMeans

wss <- c()  
for (i in 2:15) wss[i]<- sum(kmeans(d, centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")

#Lets take the Value of K as 4 as obtained by Elbow Plot

k_mean_cluster <- kmeans(d,4)
k_mean_cluster$centers
k_mean_cluster$cluster
print(k_mean_cluster)

aggregate(crime1, by=list(cluster=k_mean_cluster$cluster), mean) #we compute the mean of each variables by clusters using the original data
dd <- cbind(crime1, cluster5 = k_mean_cluster$cluster)   #add the point classifications to the original data

head(dd)
plot(dd)

k_mean_cluster$cluster  # Cluster number for each of the observations
#head(k_mean_cluster$cluster, 50)

k_mean_cluster$size  #cluster size

k_mean_cluster$centers  # Cluster means

final_Cluster_info <- data.frame("City"=crime[,1], "Cluster"=k_mean_cluster$cluster)


plot(k_mean_cluster)

# Lets Perform Different Distance  Methods on the data#

d.manhat <- dist(crime1, method = "manhattan")
d.manhat

library(factoextra)

d.pearson <- get_dist(crime1, method = "pearson")
d.pearson

d.kendall <- get_dist(crime1, method = "kendall")
d.kendall



d.spearman <- get_dist(crime1, method = "spearman")
d.spearman

# Lets perform various Clustering using these distances #

library(ggpubr)

install.packages("factoextra")
library(factoextra)





sing.clust <- hclust(d.manhat, method = "single") #Single Linkage Method
fviz_dend(sing.clust)
sing.clust.cuttree <- cutree(sing.clust, k=5)
sing.clust.data <- data.frame(crime[,1],"cluster"=sing.clust.cuttree)
sing.clust.data







comp.clust <- hclust(d.manhat, method = "complete") #Complete Linkage Method



fviz_dend(comp.clust)
comp.cuttree <- cutree(comp.clust, k=5)
comp.clust.data <- data.frame(crime[,1],"cluster"=comp.cuttree)
comp.clust.data

#For Density Based Clustering
install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

#To determine the eps value: dbscan::kNNdistplot(df, k =  5)
#abline(h = 0.15, lty = 2)

dens.clust <- dbscan(d.pearson, minPts = 5, eps = 0.15)
fviz_cluster(dens.clust,data = crime1, palette ="jco", geom = "point", ggtheme = theme_classic())
dens.clust.data <- data.frame(crime[,1], "cluster"=dens.clust$cluster)
dens.clust.data
#Cluster 0 corresponds to Outliers

#Model Based Cluster

library(mclust)
model.based <- Mclust(d.pearson)
summary(model.based)

model.based$modelName #Returns the name of the model
model.based$G #Returns the total number of Clusters

fviz_mclust(model.based, "BIC",  palette = "jco")
fviz_mclust(model.based, "classification", geom = "point",palette="jco")
fviz_mclust(model.based,"uncertainty",palette = "jco")

#Fuzzy Clustering

library(cluster)

# fanny(x, k, metric = "euclidean", stand = FALSE)
# x: A data matrix or data frame or dissimilarity matrix
# k: The desired number of clusters to be generated
# metric: Metric for calculating dissimilarities between observations
# stand: If TRUE, variables are standardized before calculating the dissimilarities

fuz <- fanny(crime1, 3) #Fuzzy Cluster
fuz$clustering #Returns the Cluster for each value
fuz$membership #Returns the membership Coefficient for each value
fviz_cluster(fuz, ellipse.type = "norm", repel = TRUE,palette = "jco", ggtheme = theme_minimal(),legend = "right")
fuz.data <- data.frame(crime[,1], "cluster"=fuz$clustering)



fuz.data


#Partitioning around Medoids (PAM) Also Called K-Medoids Algorithm for Clustering

#library("cluster","factoextra")
pammodel <- pam(crime1,3, metric = "manhattan",stand = FALSE)
pammodel$medoids
pammodel$clustering
fviz_cluster(pammodel, palette="jco",repel = TRUE, ggtheme = theme_classic(), legend = "right")
pammodel.data <- data.frame(crime[,1], "Cluster"=pammodel$clustering)
pammodel.data

