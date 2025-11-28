##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(rpart)
library(caret)

## set working directory so that files can be referenced without the full path
#setwd("~/Courses/Data Analytics/Fall25/labs/lab 4/")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)

X <- wine[, 2:7]
Y <- wine[,8:13]

Type <- wine$Type

## visualize variables
pairs.panels(X,gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(data.frame(Type, X), ggplot2::aes(colour = Type))

pairs.panels(Y,gap = 0,bg = c("orange", "green", "purple")[wine$Type],pch=21)

ggpairs(data.frame(Type, Y), ggplot2::aes(colour = Type))

##PCA

pca_all <- prcomp(wine[, 2:14], center = TRUE, scale. = TRUE)

summary(pca_all)           
pca_all$rotation
pca_all$x           

# Biplot with types colored, loadings shown
autoplot(
  pca_all,
  data = wine,
  colour = 'Type',
  loadings = TRUE,
  loadings.colour = 'blue',
  loadings.label = TRUE,
  loadings.label.size = 3
)


#KNN
set.seed(123)
s.train <- sample(1:nrow(wine), 0.7 * nrow(wine)) #asked chatgpt for a better sample size 

wine.train <-wine[s.train,]
wine.test <-wine[-s.train,]

## kNN Model
train.features <- scale(wine.train[, -1])  #excluding type
test.features  <- scale(wine.test[, -1],
                        center = attr(train.features, "scaled:center"),
                        scale = attr(train.features, "scaled:scale"))

train.labels <- wine.train$Type
test.labels  <- wine.test$Type

## Train kNN model (k = 3 as example)
knn.predicted <- knn(train = train.features,
                     test = test.features,
                     cl = train.labels,
                     k = 3)

## Confusion matrix
knn_cm <- confusionMatrix(knn.predicted, test.labels)
print(knn_cm)
conf_matrix <- table(Predicted = knn.predicted, Actual = test.labels)
print(conf_matrix)

## Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

#DECISION TREE
scores <- data.frame(pca_all$x[, 1:2], Type = wine$Type)
pc.train <- scores[s.train, ]
pc.test  <- scores[-s.train, ]

tree_model <- rpart(Type ~ PC1 + PC2, data = pc.train, method = "class")
tree_pred  <- predict(tree_model, newdata = pc.test, type = "class")

tree_cm <- confusionMatrix(tree_pred, pc.test$Type)
cat("\n=== Decision Tree on First 2 PCs ===\n")
print(tree_cm$table)
print(tree_cm$overall['Accuracy'])
print(tree_cm$byClass[, c("Precision","Recall","F1")])

comparison <- data.frame(
  Model = c("kNN (All Variables)", "Decision Tree (PC1 & PC2)"),
  Accuracy = c(knn_cm$overall["Accuracy"], tree_cm$overall["Accuracy"]),
  Precision = c(mean(knn_cm$byClass[, "Precision"], na.rm = TRUE),
                mean(tree_cm$byClass[, "Precision"], na.rm = TRUE)),
  Recall = c(mean(knn_cm$byClass[, "Recall"], na.rm = TRUE),
             mean(tree_cm$byClass[, "Recall"], na.rm = TRUE)),
  F1 = c(mean(knn_cm$byClass[, "F1"], na.rm = TRUE),
         mean(tree_cm$byClass[, "F1"], na.rm = TRUE))
)
print(comparison)
