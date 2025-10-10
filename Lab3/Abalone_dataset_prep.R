  ####################################
##### Abalone Data Preparation #####
####################################

library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

# read dataset
abalone.data <- read.csv("abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
#abalone.data$age.group[abalone.data$rings<=8] <- "young"
#abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
#abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"


sqrt(4176) #~65, #of neighbors 

ggplot(abalone.data, aes(x = length, y = diameter, colour = age.group)) +
  geom_point()

ggplot(abalone.data, aes(x = shell_weight, y = whole_weight, colour = age.group)) +
  geom_point()

pairs.panels(abalone.data[2:8],gap = 0,bg = c("pink", "green", "blue")[abalone.data$age.group],pch=21)


## GGally ## psych scatterplot matrix
ggpairs(abalone.data, ggplot2::aes(colour = age.group))

##EXERCISE 1

k = 3

abalone.km <- kmeans(abalone.data[2:8], centers = k)

wcss <- abalone.km$tot.withinss
wcss

assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(abalone.data[2:8], aes(x = length, y = diameter, colour = assigned.clusters)) +
  geom_point()

ggplot(abalone.data[2:8], aes(x = shell_weight, y = whole_weight, colour = assigned.clusters)) +
  geom_point()

k.list <- c(1,2,3,4,5,6,7)

wcss.list <- c()
si.list <- c()

for (k in k.list) {
  
  abalone.km <- kmeans(abalone.data[2:8], centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  if (k>1){
    
    si <- silhouette(abalone.km$cluster, dist(abalone.data[2:8]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
}

plot(k.list,wcss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

wcss.list

diff(wcss.list)

diff(diff(wcss.list))

##EXERCISE 2
