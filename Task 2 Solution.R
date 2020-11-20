# Installing packages

install.packages('factoextra')
install.packages('cluster')
install.packages('fpc')
install.packages('NbClust')
install.packages('clValid')
install.packages('magrittr')
install.packages('clustertend')

# Calling libraries

Packages <- c("factoextra", "cluster", "fpc", "NbClust", 'clValid', 'magrittr', 'clustertend')

lapply(Packages, library, character.only = TRUE)

#Reading the data

setwd("C:\\Users\\MOHIT\\Desktop\\TSF-GRIPS Internship\\Task 2")

data <- read.csv("iris.csv")

data<- data[,-c(6)]

data

# To standarize the variables 
data = scale(data) 

# Assessing cluster tendency
library(clustertend)
# Compute Hopkins statistic for the dataset
## values > .5 means it is not clusterable
## values < .5 means it is clusterable, closer to 0 better the data

set.seed(123)
hopkins(data, n = nrow(data)-1)
#Since the H value = 0.1951 which is far below the threshold 0.5, it is highly clusterable

# different way to calculate hopkins test
library(factoextra)
res <- get_clust_tendency(data, n = nrow(data)-1, graph = FALSE)
res$hopkins_stat



# K-mean - Determining optimal number of clusters
# NbClust Package : 30 indices to determine the number of clusters in a dataset
# If index = 'all' - run 30 indices to determine the optimal no. of clusters
# If index = "silhouette" - It is a measure to estimate the dissimilarity between clusters.
# A higher silhouette width is preferred to determine the optimal number of clusters


library(NbClust)
##Method I: using silhouette method
nb <- NbClust(data,  distance = "euclidean", min.nc=2, max.nc=15, 
              method = "kmeans",index = "silhouette")

nb$All.index## maximum value of silhouette shows best number of clusters
nb$Best.nc

# Here we can see best or optimal number of clusters is 2

#Method II : Same Silhouette Width analysis with fpc package
library(fpc)
# maximum value of silhouette shows best number of clusters
pamkClus <- pamk(data, krange = 2:15, criterion="multiasw", ns=2, critout=TRUE)
pamkClus$nc
cat("number of clusters estimated by optimum average silhouette width:", pamkClus$nc, "\\\\n")

#Method III : Scree plot to determine the number of clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(data,centers=i)$withinss)
} 
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#From this plot we can visually see the number of optimal clusters is 2

##Best Method IV: using all 30 ways of measure
nb <- NbClust(data,  distance = "euclidean", min.nc=2, max.nc=15, 
              method = "kmeans",index = "all")

########################## Running k means #######################################

# K-Means Cluster Analysis
fit <- kmeans(data,3)
plot(data,col=fit$cluster,pch=16) 


### kmeans clustering with a better visualization

# K-means clustering
# nstart means it initiates multiple initial configuaration and reports the best one
km.res <- eclust(data, "kmeans", k = 3, nstart = 25, graph = FALSE)

# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", frame.type = "norm")


## another way of finding number of clusters and running kmeans
kmeans = kmeansruns(data,3,runs=100) ## for a single value
kmeans=kmeansruns(data,krange = 1:12,runs=100) ## gives a range of value and R will find best value of k
plot(data,col=kmeans$cluster,pch=16) 

####################### Clustering Validation #################################################

#Internal clustering validation, which use the internal information of the 
#clustering process to evaluate the goodness of a clustering structure. 
#It can be also used for estimating the number of clusters and the appropriate clustering algorithm.


## Connectivity - ranges from 0 to Inf; choose the one with minimum value 
## Average Silhouette width - ranges from -1 to 1; choose the one with maximum value 
## Dunn index - ranges from 0 to Inf; choose the one with maximum value 


# Internal Validation
clmethods <- c("kmeans")
internval <- clValid(data, nClust = 2:6, clMethods = clmethods, validation = "internal")

# Summary
summary(internval)
optimalScores(internval)

# Stability measure Validation
clmethods <- c("kmeans")
stabval <- clValid(data, nClust = 2:5, clMethods = clmethods, validation = "stability")

# Display only optimal Scores
summary(stabval)
optimalScores(stabval)

#The average proportion of non-overlap (APN)
#The average distance (AD)
#The average distance between means (ADM)
#The figure of merit (FOM)
### Smaller values indicate stable clusters


## You have to run kmeans clustering with a better visualization method to make it work

fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())


# Silhouette width of observations
sil <- km.res$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0) 
sil[neg_sil_index, , drop = FALSE]

##################################################################


# External Clustering Validation
library(fpc)

# K-Means Cluster Analysis
fit <- kmeans(data,3)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(data), species, fit$cluster)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats$corrected.rand ## value should be maximized
clust_stats$vi  ## value should be minimized
