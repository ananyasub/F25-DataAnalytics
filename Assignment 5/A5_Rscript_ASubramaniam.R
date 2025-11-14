library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(rpart)
library(caret)
library(dplyr)
library(randomForest)
install.packages("rpart.plot")
library(rpart.plot)

nyc.data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")
View(nyc.data)

manhattan <- nyc.data %>% filter(BOROUGH == 1)
head(manhattan)

clean.manhattan <- manhattan[manhattan$SALE.PRICE > 1000, ]

ggplot(clean.manhattan, aes(x = SALE.PRICE)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  scale_x_log10() + #compresses larger values and stretches smaller ones 
  ggtitle("Distribution of Sale Price")

ggplot(clean.manhattan, aes(y = SALE.PRICE)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  scale_y_log10() +
  ggtitle("Sale Price Boxplot (Log Scale)") +
  ylab("Sale Price (log scale)") +
  theme_minimal()

manhattan.model <- lm(SALE.PRICE ~ LAND.SQUARE.FEET, data = clean.manhattan)
summary(manhattan.model)

manhattan.model1 <- lm(SALE.PRICE ~ ZIP.CODE, data = clean.manhattan)
summary(manhattan.model1)

manhattan.model2 <- lm(SALE.PRICE ~ RESIDENTIAL.UNITS, data = clean.manhattan)
summary(manhattan.model2)

model.df <- clean.manhattan %>%
  select(NEIGHBORHOOD, SALE.PRICE, GROSS.SQUARE.FEET, LAND.SQUARE.FEET, YEAR.BUILT) %>%
  filter(complete.cases(.)) %>%
  mutate(
    NEIGHBORHOOD = factor(NEIGHBORHOOD),
    SALE.PRICE        = as.numeric(SALE.PRICE),
    GROSS.SQUARE.FEET = as.numeric(GROSS.SQUARE.FEET),
    LAND.SQUARE.FEET  = as.numeric(LAND.SQUARE.FEET),
    YEAR.BUILT        = as.numeric(YEAR.BUILT)
  ) %>%
  droplevels()

model.df <- na.omit(model.df)

model.df$NEIGHBORHOOD <- factor(model.df$NEIGHBORHOOD)

set.seed(123)
index <- createDataPartition(model.df$NEIGHBORHOOD, p = 0.7, list = FALSE)

train <- model.df[index, ]
train <- train %>%
  mutate(
    SALE.PRICE = as.numeric(SALE.PRICE),
    GROSS.SQUARE.FEET = as.numeric(GROSS.SQUARE.FEET),
    LAND.SQUARE.FEET = as.numeric(LAND.SQUARE.FEET),
    YEAR.BUILT = as.numeric(YEAR.BUILT)
  )
test  <- model.df[-index, ]
test <- test %>%
  mutate(
    SALE.PRICE = as.numeric(SALE.PRICE),
    GROSS.SQUARE.FEET = as.numeric(GROSS.SQUARE.FEET),
    LAND.SQUARE.FEET = as.numeric(LAND.SQUARE.FEET),
    YEAR.BUILT = as.numeric(YEAR.BUILT)
  )

num_cols <- c("SALE.PRICE", "GROSS.SQUARE.FEET", "LAND.SQUARE.FEET", "YEAR.BUILT")
train.X <- scale(train[, num_cols, drop = FALSE])                 # matrix
test.X  <- scale(test[,  num_cols, drop = FALSE],
                 center = attr(train.X, "scaled:center"),
                 scale  = attr(train.X, "scaled:scale"))

train.y <- train$NEIGHBORHOOD
test.y  <- test$NEIGHBORHOOD

ks <- c(3,5,7,9,11)
accs <- sapply(ks, function(k) {
  p <- class::knn(train = train.X, test = test.X, cl = train.y, k = k)
  mean(p == test.y)
})
best.k <- ks[which.max(accs)]
best.k

knn.pred <- class::knn(train = train.X, test = test.X, cl = train.y, k = best.k)
conf_matrix <- table(Predicted = knn.pred, Actual = test.y)
print(conf_matrix)


rf.fit <- randomForest(NEIGHBORHOOD ~ ., data = train, ntree = 300)

rf.pred <- predict(rf.fit, newdata = test)

confusionMatrix(rf.pred, test.y)

tree.fit <- rpart(
  NEIGHBORHOOD ~ SALE.PRICE + GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT,
  data   = train,
  method = "class",
  control = rpart.control(cp = 0.001, minbucket = 20, xval = 10)
)

rpart.plot(tree.fit)
tree.fit.predicted <- predict(tree.fit, test, type = "class")
table(Predicted = tree.fit.predicted, Actual = test$NEIGHBORHOOD)
cm <- confusionMatrix(tree.fit.predicted, test$NEIGHBORHOOD)
print(cm)

#BOROUGH 2 
borough2 <- nyc.data %>% filter(BOROUGH == 2)
head(borough2)

clean.bourough2 <- borough2[borough2$SALE.PRICE > 1000, ]


borough2.model <- lm(SALE.PRICE ~ LAND.SQUARE.FEET, data = clean.bourough2)
summary(borough2.model)

borough2.model1 <- lm(SALE.PRICE ~ ZIP.CODE, data = clean.bourough2)
summary(borough2.model1)

borough2.model2 <- lm(SALE.PRICE ~ RESIDENTIAL.UNITS, data = clean.bourough2)
summary(borough2.model2)

borough2 <- nyc.data %>% filter(BOROUGH == 2)

clean.b2 <- borough2 %>%
  mutate(
    SALE.PRICE        = as.numeric(SALE.PRICE),
    GROSS.SQUARE.FEET = as.numeric(GROSS.SQUARE.FEET),
    LAND.SQUARE.FEET  = as.numeric(LAND.SQUARE.FEET),
    YEAR.BUILT        = as.numeric(YEAR.BUILT)
  ) %>%
  filter(SALE.PRICE > 1000) %>%
  select(NEIGHBORHOOD, SALE.PRICE, GROSS.SQUARE.FEET, LAND.SQUARE.FEET, YEAR.BUILT) %>%
  filter(complete.cases(.)) %>%
  mutate(NEIGHBORHOOD = factor(NEIGHBORHOOD))

# 1) Drop very-rare classes (keep classes with >= 5 samples; adjust as needed)
freq <- table(clean.b2$NEIGHBORHOOD)
keep_lvls <- names(freq[freq >= 5])
b2 <- clean.b2 %>% filter(NEIGHBORHOOD %in% keep_lvls) %>% droplevels()

set.seed(123)
idx <- createDataPartition(b2$NEIGHBORHOOD, p = 0.7, list = FALSE)
train <- b2[idx, ]
test  <- b2[-idx, ]

num_cols <- c("SALE.PRICE","GROSS.SQUARE.FEET","LAND.SQUARE.FEET","YEAR.BUILT")
train.X <- scale(train[, num_cols, drop = FALSE])
test.X  <- scale(test[,  num_cols, drop = FALSE],
                 center = attr(train.X, "scaled:center"),
                 scale  = attr(train.X, "scaled:scale"))

train.y <- droplevels(train$NEIGHBORHOOD)
test.y  <- droplevels(test$NEIGHBORHOOD)

# 2) Tune k and align factor levels before confusionMatrix
ks <- c(3,5,7,9,11)
accs <- sapply(ks, function(k) {
  p <- class::knn(train = train.X, test = test.X, cl = train.y, k = k)
  p <- factor(p, levels = levels(test.y))   # align levels
  mean(p == test.y)
})
best.k <- ks[which.max(accs)]

knn.pred <- class::knn(train = train.X, test = test.X, cl = train.y, k = best.k)
knn.pred <- factor(knn.pred, levels = levels(test.y))             # <-- critical
confusionMatrix(knn.pred, test.y) 

rf.fit <- randomForest(NEIGHBORHOOD ~ ., data = train, ntree = 300)

rf.pred <- predict(rf.fit, newdata = test)

confusionMatrix(rf.pred, test.y)

tree.fit <- rpart(
  NEIGHBORHOOD ~ SALE.PRICE + GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT,
  data   = train,
  method = "class",
  control = rpart.control(cp = 0.001, minbucket = 20, xval = 10)
)

rpart.plot(tree.fit)
tree.fit.predicted <- predict(tree.fit, test, type = "class")
table(Predicted = tree.fit.predicted, Actual = test$NEIGHBORHOOD)
cm <- confusionMatrix(tree.fit.predicted, test$NEIGHBORHOOD)
print(cm)
