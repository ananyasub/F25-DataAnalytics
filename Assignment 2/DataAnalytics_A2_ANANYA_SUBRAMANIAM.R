library(readr)
library(ggplot2)
library(GGally)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(class)
library(caret)

epi.data <- read.csv("epi_results_2024_pop_gdp_v2.csv")

View(epi.data)

region1 <- subset(epi.data, region == "Southern Asia")
region2 <- subset(epi.data, region == "Latin America & Caribbean")

ggplot(region1, aes(y = gdp)) +
  geom_boxplot(fill = "pink") +
  labs(title = "GDP Distribution - Southern Asia")

ggplot(region1, aes(x=gdp)) + 
  geom_histogram(aes(y = ..density..),bins = 25, fill = "pink", color = "black") + #scaled 
  geom_density(color = "green", size = 1) +
  labs(title = "GDP Histogram - Southern Asia")


ggplot(region2, aes(y = gdp)) +
  geom_boxplot(fill = "darkorchid") +
  labs(title = "GDP Distribution - Latin America & Caribbean")

ggplot(region2, aes(x=gdp)) + 
  geom_histogram(aes(y = ..density..),bins = 25, fill = "darkorchid", color = "black") + #scaled 
  geom_density(color = "green", size = 1) +
  labs(title = "GDP Histogram - Latin America & Caribbean")


qqPlot(region1$gdp, region2$gdp,
       main = "QQ Plot of GDP between Southern Asia and Latin America & Caribbean",
       xlab = "Southern Asian GDP Quantiles",
       ylab = "Latin America & Caribbean GDP Quantiles")

#2 - Linear Models 

gdp <- epi.data$gdp

ggplot(epi.data, aes(x = gdp, y = EPI.new, colour = region)) +
  geom_point()

epi.data$log_gdp <- log10(gdp)

#created linear model of EPI.new ~ gdp
lin.mod1 <- lm(EPI.new~log_gdp,epi.data)

summary(lin.mod1)

ggplot(epi.data, aes(x = log_gdp, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



### subset by regions
region1$log_gdp <- log10(region1$gdp)

summary(epi.data$region)

##convert region from strings to factors
epi.data$region <- as.factor(epi.data$region)

summary(epi.data$region)

epi.data.subset <- epi.data[ epi.data$region %in% c("Southern Asia"),]

ggplot(epi.data.subset, aes(x = log_gdp, y = EPI.new, colour = region, label=country)) +
  geom_point() + geom_text(hjust=0, vjust=0)

lin.mod2 <- lm(log_gdp~EPI.new,epi.data.subset)

summary(lin.mod2)

ggplot(epi.data.subset, aes(x = log_gdp, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#3. Classification (kNN)
epi.clean <- epi.data[complete.cases(epi.data[, c("region", "EPI.new", "ECO.new", "BDH.new")]), ]

# Select 3 variables: EPI.new, ECO.new, BDH.new
vars1 <- c("EPI.new", "ECO.new", "BDH.new", "region")
model.data1 <- epi.clean[, vars1]

# Split into train/test (70/30 split)
set.seed(123)  # for reproducibility
split.rat <- 0.7
train.indexes <- sample(nrow(model.data1), split.rat * nrow(model.data1))
train1 <- model.data1[train.indexes, ]
test1 <- model.data1[-train.indexes, ]

cat("=== MODEL 1: EPI.new, ECO.new, BDH.new ===\n\n")

# Try different k values
k_values <- c(3, 5, 7, 9, 11)
best_accuracy <- 0
best_k <- 3

for(k in k_values) {
  # Train kNN model using caret
  mod.knn <- train(region ~ ., data = train1, method = "knn", 
                   tuneGrid = data.frame(k = k),
                   trControl = trainControl(method = "none"))
  
  # Predict on test set
  knn.test.predicted <- predict(mod.knn, test1[, -4])
  
  # Create confusion matrix
  conf.mat <- as.matrix(table(Actual = test1$region, Predicted = knn.test.predicted))
  
  # Calculate accuracy
  accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
  
  cat("\n--- k =", k, "---\n")
  print(conf.mat)
  cat("\nAccuracy:", round(accuracy, 4), "\n")
  cat("Correctly classified:", sum(diag(conf.mat)), "out of", sum(conf.mat), "\n")
  
  # Track best k
  if(accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_k <- k
  }
}

epi.clean2 <- epi.data[complete.cases(epi.data[, c("region", "MKP.new", "MHP.new", "MPE.new")]), ]

vars2 <- c("MKP.new", "MHP.new", "MPE.new", "region")
model.data2 <- epi.clean2[, vars2]

set.seed(123)  # same seed for fair comparison
train.indexes2 <- sample(nrow(model.data2), split.rat * nrow(model.data2))
train2 <- model.data2[train.indexes2, ]
test2 <- model.data2[-train.indexes2, ]

mod.knn2 <- train(region ~ ., data = train2, method = "knn", 
                  tuneGrid = data.frame(k = best_k),
                  trControl = trainControl(method = "none"))

# Predict on test set
knn.test.predicted2 <- predict(mod.knn2, test2[, -4])

# Create confusion matrix
conf.mat2 <- as.matrix(table(Actual = test2$region, Predicted = knn.test.predicted2))

accuracy2 <- sum(diag(conf.mat2)) / sum(conf.mat2)

cat("\n--- k =", best_k, "---\n")
print(conf.mat2)
cat("\nAccuracy:", round(accuracy2, 4), "\n")
cat("Correctly classified:", sum(diag(conf.mat2)), "out of", sum(conf.mat2), "\n")

cat("Model 1 (EPI.new, ECO.new, BDH.new) Accuracy:", round(best_accuracy, 4), "\n")
cat("Model 2 (MKP.new, MHP.new, MPE.new) Accuracy:", round(accuracy2, 4), "\n\n")

if(best_accuracy > accuracy2) {
  cat("CONCLUSION: Model 1 performs better with", 
      round((best_accuracy - accuracy2)*100, 2), 
      "percentage points higher accuracy, suggesting that overall environmental performance indicators (EPI, ECO, BDH) are more effective at distinguishing regions than marine and habitat protection metrics.\n")
} else {
  cat("CONCLUSION: Model 2 performs better with", 
      round((accuracy2 - best_accuracy)*100, 2), 
      "percentage points higher accuracy, suggesting that marine and habitat protection metrics (MKP, MHP, MPE) are more effective at distinguishing regions than overall environmental performance indicators.\n")
}

