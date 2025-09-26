library(ggplot2)

housing.data <- read.csv("NY-House-Dataset.csv", header=TRUE)
View(housing.data)

housing.price <- housing.data$PRICE
housing.sqft <- housing.data$PROPERTYSQFT

## NA values
na.indices <- is.na(housing.price) 

## drop NAs
housing.price.compl <- housing.price[!na.indices]

housing.data.subset <- housing.data[housing.price < 2000000, ] #2 million


mod1 <- lm(PRICE ~ PROPERTYSQFT, data = housing.data.subset)

summary(mod1)

ggplot(housing.data.subset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Price vs PropertySqFt with Fit Line")

ggplot(mod1, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Model 1)")

########price vs beds

housing.data.subset <- housing.data[housing.price < 2000000 & housing.data$BEDS < 19, ] 


mod2 <- lm(PRICE ~ BEDS, data = housing.data.subset)

summary(mod2)

ggplot(housing.data.subset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Price vs Beds with Fit Line")

ggplot(mod2, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Model 2)")

########price vs baths

housing.data.subset <- housing.data[housing.price < 2000000, ] 


mod3 <- lm(PRICE ~ BATH, data = housing.data.subset)

summary(mod3)

ggplot(housing.data.subset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Price vs Baths with Fit Line")

ggplot(mod3, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Model 3)")
