# TSF-Task6-Prediction-using-Decision-Tree-Algorithm-Level---Intermediate-
#Prediction using Decision Tree Algorithm (Level - Intermediate)
#Create the Decision Tree classifier and visualize it graphically.
#The purpose is if we feed any new data to this classifier, it would be able to predict the right class accordingly.

library(ggplot2)
install.packages("cluster")
library(cluster)
library(tidyverse)
install.packages("factoextra")
install.packages("gridExtra")
library(factoextra)
require("datasets")
install.packages("NbClust")
library(NbClust)

head(iris)
iris.new <- iris[,c(1,2,3,4)]
iris.class <- iris[,c(5)]
head(iris.class)
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
  
}

iris.new$Sepal.Length <- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width <- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length <- normalize(iris.new$Petal.Length)
iris.new$Petal.Length <- normalize(iris.new$Petal.Length)

kmeans3 <- kmeans(iris.new,3)

kmeans3$size
kmeans3$cluster
#par(mfrow=c(2,2),mar=c(5,4,2,2))
plot(iris.new[c(1,2)],col=kmeans3$cluster)

#elbow method
fviz_nbclust(iris.new, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(iris.new, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(iris.new, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
NbClust(data = iris.new, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "ward.D")
