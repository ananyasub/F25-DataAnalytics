library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(rpart)
library(caret)

wine <- read.csv("wine.data")

names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                 "Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                 "Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

names(wine) <- make.names(names(wine))

wine$Type <- as.factor(wine$Type)
wine <- wine[-which(wine$Type==3),]
wine$Type <- droplevels(wine$Type)
wine$Type <- as.factor(wine$Type)

N <- nrow(wine)
train.indexes <- sample(N, 0.8 * N)

train <- wine[train.indexes, ]
test  <- wine[-train.indexes, ]

ggpairs(train, ggplot2::aes(colour = train$Type))
ggplot(train, aes(x = Malic.acid, y = Proline, colour = Type)) +
  geom_point()

# Linear 
svm.mod0 <- svm(Type ~ Malic.acid + Proline, data = train, kernel = "linear")
svm.mod0
plot(svm.mod0,
     data = train,
     formula = Proline ~ Malic.acid,
     svSymbol = "x",
     dataSymbol = "o")

train.pred <- predict(svm.mod0, train)
cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

make.grid = function(X, n = 75) {
  grange = apply(X, 2, range)
  X1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  X2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Malic.acid = X1, Proline = X2)
}

X <- train[,3:4]
Y <- as.numeric(train$Type)
Y[Y==2] <- -1

xgrid = make.grid(X)
ygrid = predict(svm.mod0, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)

points(X, col = Y + 3, pch = 19)
points(X[svm.mod0$index,], pch = 5, cex = 2)

linear.metrics <- data.frame(
  Model = "Linear SVM",
  Class = rownames(data.frame(precision, recall, f1)),
  Precision = precision,
  Recall = recall,
  F1 = f1
)
linear.metrics


#Polynomial Kernel

svm.mod1 <- svm(Type ~ Malic.acid + Proline, data = train, kernel = 'radial')

plot(svm.mod1, train, Malic.acid~Proline)

train.pred <- predict(svm.mod1, train)

ygrid = predict(svm.mod1, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(X, col = Y + 1, pch = 19)
points(X[svm.mod0$index,], pch = 5, cex = 2)
points(X, col = Y + 3, pch = 19)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

#data.frame(precision, recall, f1)

rbf.metrics <- data.frame(
  Model = "RBF SVM",
  Class = rownames(data.frame(precision, recall, f1)),
  Precision = precision,
  Recall = recall,
  F1 = f1
)
rbf.metrics



## Tuned SVM - polynomial
gamma.range <- seq(0.1,10, .1)
gamma.range

Cost.range <- seq(1,20, 1)
Cost.range

tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial',gamma = gamma.range, cost = Cost.range)
tuned.svm

svm.mod2 <- svm(Type ~ ., data = train, kernel = 'polynomial', gamma = 0.1, cost = 1)

svm.mod2

train.pred <- predict(svm.mod2, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

poly.metrics <- data.frame(
  Model = "Polynomial SVM",
  Class = rownames(data.frame(precision, recall, f1)),
  Precision = precision,
  Recall = recall,
  F1 = f1
)
poly.metrics


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

precision <- c(
  knn_cm$byClass["Pos Pred Value"],   # Precision for class 1
  knn_cm$byClass["Neg Pred Value"]    # Precision for class 2
)

recall <- c(
  knn_cm$byClass["Sensitivity"],      # Recall for class 1
  knn_cm$byClass["Specificity"]       # Recall for class 2
)

f1 <- 2 * precision * recall / (precision + recall)

knn.metrics <- data.frame(
  Model = "kNN",
  Class = c("1","2"),
  Precision = precision,
  Recall = recall,
  F1 = f1
)
knn.metrics

comparison <- rbind(linear.metrics, rbf.metrics, poly.metrics, knn.metrics)
comparison



