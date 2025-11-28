###################################
##### Clustering with Iris v2 #####
###################################

library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

iris.data <- iris

## scale variables
# iris.data[-5] <- scale(iris.data[-5])


## ggplo2 scatterplots
ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

ggplot(iris.data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

ggplot(iris.data, aes(x = Petal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

ggplot(iris.data, aes(x = Sepal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


## psych scatterplot matrix
pairs.panels(iris.data[,-5],gap = 0,bg = c("pink", "green", "blue")[iris.data$Species],pch=21)


## GGally ## psych scatterplot matrix
ggpairs(iris.data, ggplot2::aes(colour = Species))


### K-Means ###

k = 3

iris.km <- kmeans(iris.data[,-5], centers = k)
iris.km$withinss
iris.km$tot.withinss

## Within clusters sum of squares
wcss <- iris.km$tot.withinss
wcss

## get and plot clustering output 
assigned.clusters <- as.factor(iris.km$cluster)

ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
geom_point()

iris.dist <- as.matrix(dist(iris[-5]))

## Silhouette Plot
sil <- silhouette(iris.km$cluster, dist(iris.data[-5])) #dist matrix = distance between all points and each other
fviz_silhouette(sil)

## run tests with multiple k values and plot WCSS
k.list <- c(1,2,3,4,5,6,7)

wcss.list <- c()
si.list <- c()

for (k in k.list) {
  
  iris.km <- kmeans(iris.data[,-5], centers = k)
  
  wcss <- iris.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  if (k>1){
    
    si <- silhouette(iris.km$cluster, dist(iris.data[-5]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
}

plot(k.list,wcss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

wcss.list

diff(wcss.list)

diff(diff(wcss.list))


### Partitioning Around Medoids ###

k = 3

iris.pam <- pam(iris.data[-5], k)


## get and plot clustering output 
assigned.clusters <- as.factor(iris.pam$cluster)

ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

## Silhouette Plot
sil <- silhouette(iris.pam$cluster, dist(iris.data[-5]))
fviz_silhouette(sil)

## run tests with multiple k values and plot sum of dissimilarities (sum of distances)
k.list <- c(1,2,3,4,5,6,7)

sumdiss.list <- c()
si.list <- c()

for (k in k.list) {
  
  iris.pam <- pam(iris.data[,-5], k)
  
  sumdiss <- iris.pam$objective[1]
  
  sumdiss.list <- c(sumdiss.list,sumdiss)
  
  if (k>1){
    si <- silhouette(iris.pam$cluster, dist(iris.data[-5]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
  
}

plot(k.list,sumdiss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

### Hierarchical Clustering ###

iris.dist <- dist(iris.data[-5])

iris.hclust <- hclust(iris.dist)

iris_species <- sort(levels(iris[,5]), decreasing = TRUE)

plot(iris.hclust)


#### Custom Dendrogram #########
## https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
#######
dend <- as.dendrogram(iris.hclust)

# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3)  #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))

plot(dend, 
     main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))

legend("topleft", legend = iris_species, fill = rainbow_hcl(3))



############################################################
############################################################