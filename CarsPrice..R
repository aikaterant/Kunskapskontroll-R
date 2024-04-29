getwd()
# Read the CSV file into R
cars_data <- read.csv("CarsDatafinal.csv", header = TRUE)

# Check the first few rows of the dataframe to confirm it's loaded correctly
head(cars_data)

# Load CSV data into R
cars_data <- read.csv("CarsDatafinal.csv")

# Check the structure of the data
str(cars_data)

# Assuming 'cars_data' is your dataframe

# Remove unnecessary columns
cars_data <- subset(cars_data, select = -c(Model, Link.to.advertisement))
# Check the last few rows to see if anything looks unusual
tail(cars_data)

# Remove rows where all entries are NA
cars_data <- na.omit(cars_data)
#Check the structure of the cleaned data
str(cars_data)
# Print a summary of the cleaned data
summary(cars_data)
# Summarize missing values in each column
sum(is.na(cars_data))
#Convert Data Types
cars_data$Year <- as.factor(cars_data$Year)
cars_data$Fuel <- as.factor(cars_data$Fuel)
cars_data$Gearbox <- as.factor(cars_data$Gearbox)
plot(cars_data$Year, cars_data$Price, main="Price vs Year", xlab="Year", ylab="Price")
summary(lm(Price ~ Year + Mileage + Horsepower, data=cars_data))
model <- lm(Price ~ Year + Mileage + Horsepower, data=cars_data)
summary(model)

# Load necessary library
library(car)

# Calculate VIF
vif_model <- vif(lm(Price ~ Year + Mileage + Horsepower, data = cars_data))
print(vif_model)


install.packages("lmtest")
library(lmtest)
bptest(model)

cars_data$Log_Price <- log(cars_data$Price)
model_log <- lm(Log_Price ~ Year + Mileage + Horsepower, data=cars_data)
summary(model_log)


# Load necessary packages
library(sandwich)
library(lmtest)
# Fit the model with log-transformed price
model_log <- lm(Log_Price ~ Year + Mileage + Horsepower, data = cars_data)
# Calculate robust standard errors
robust_se <- coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))
# Print the summary with robust standard errors
print(robust_se)

# Install packages if not already installed
if (!require("car")) install.packages("car", dependencies=TRUE)
if (!require("lmtest")) install.packages("lmtest", dependencies=TRUE)
if (!require("sandwich")) install.packages("sandwich", dependencies=TRUE)

cars_data$Log_Price <- log(cars_data$Price)
model_log <- lm(Log_Price ~ Year + Mileage + Horsepower, data=cars_data)
summary(model_log)
# Fit the model with log-transformed price
model_log <- lm(Log_Price ~ Year + Mileage + Horsepower, data = cars_data)
# Calculate robust standard errors
robust_se <- coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))
# Print the summary with robust standard errors
print(robust_se)

# Load necessary libraries
library(car)
library(lmtest)
library(sandwich)



# Install the necessary packages
install.packages("sandwich")
install.packages("lmtest")

# Load the packages
library(sandwich)
library(lmtest)

# Fit the model with log-transformed price
model_log <- lm(Log_Price ~ Year + Mileage + Horsepower, data = cars_data)

# Calculate robust standard errors
robust_se <- coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))

# Print the summary with robust standard errors
print(robust_se)


library(sandwich)
library(lmtest)

model_robust <- lm(Price ~ Year + Mileage + Horsepower, data=cars_data)
coeftest(model_robust, vcov = vcovHC(model_robust, type = "HC1"))


# Assuming heteroscedasticity might be linked to Mileage
weights <- 1 / cars_data$Mileage
model_wls <- lm(Price ~ Year + Mileage + Horsepower, data = cars_data, weights = weights)
summary(model_wls)


# Calculate residuals from the model
residuals <- residuals(model_wls)

# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals)

# Print the results of the Shapiro-Wilk test
print(shapiro_test)


# Applying square root transformation to the 'Price' variable
cars_data$Price_sqrt <- sqrt(cars_data$Price)
# Fit the linear model using the transformed 'Price' variable
model_sqrt <- lm(Price_sqrt ~ Year + Mileage + Horsepower, data = cars_data)
# Summarize the model to look at coefficients and overall fit
summary(model_sqrt)
# Checking normality of residuals
shapiro.test(residuals(model_sqrt))
# Plot residuals to check for patterns
plot(residuals(model_sqrt) ~ fitted(model_sqrt))
abline(h = 0, col = "red")

# QQ plot to check for normality visually
qqnorm(residuals(model_sqrt))
qqline(residuals(model_sqrt), col = "red")



# Applying inverse transformation to the 'Price' variable
cars_data$Price_inv <- 1 / cars_data$Price
# Fit the linear model using the inversely transformed 'Price' variable
model_inv <- lm(Price_inv ~ Year + Mileage + Horsepower, data = cars_data)
# Summarize the model to look at coefficients and overall fit
summary(model_inv)
# Checking normality of residuals
shapiro.test(residuals(model_inv))
# Plot residuals to check for patterns
plot(residuals(model_inv) ~ fitted(model_inv))
abline(h = 0, col = "red")
# QQ plot to check for normality visually
qqnorm(residuals(model_inv))
qqline(residuals(model_inv), col = "red")




# Assuming you have `model_log`, `model_sqrt`, `model_inv` already fitted
# Use AIC and BIC to compare models
aic_log <- AIC(model_log)
aic_sqrt <- AIC(model_sqrt)
aic_inv <- AIC(model_inv)

bic_log <- BIC(model_log)
bic_sqrt <- BIC(model_sqrt)
bic_inv <- BIC(model_inv)

# Compare R-squared values
r_squared_log <- summary(model_log)$r.squared
r_squared_sqrt <- summary(model_sqrt)$r.squared
r_squared_inv <- summary(model_inv)$r.squared

# Assuming `model_inv` is the chosen model based on previous step
# Calculate VIF for the chosen model
library(car)
vif_model_inv <- vif(model_inv)
print(vif_model_inv)

# Run Breusch-Pagan test again for heteroscedasticity
library(lmtest)
bptest(model_inv)



# AIC and BIC for original model
aic_original <- AIC(model_robust)
bic_original <- BIC(model_robust)

# AIC and BIC for log-transformed model
aic_log <- AIC(model_log)
bic_log <- BIC(model_log)

# AIC and BIC for square root-transformed model
aic_sqrt


# AIC and BIC for original model
aic_original <- AIC(model_robust)
bic_original <- BIC(model_robust)

# Print AIC and BIC for the original model
print(paste("Original Model AIC:", aic_original))
print(paste("Original Model BIC:", bic_original))

# AIC and BIC for log-transformed model
aic_log <- AIC(model_log)
bic_log <- BIC(model_log)

# Print AIC and BIC for the log-transformed model
print(paste("Log Model AIC:", aic_log))
print(paste("Log Model BIC:", bic_log))

# Fit and calculate AIC and BIC for square root-transformed model
model_sqrt <- lm(sqrt(Price) ~ Year + Mileage + Horsepower, data=cars_data)
aic_sqrt <- AIC(model_sqrt)
bic_sqrt <- BIC(model_sqrt)

# Print AIC and BIC for the square root-transformed model
print(paste("Square Root Model AIC:", aic_sqrt))
print(paste("Square Root Model BIC:", bic_sqrt))

# Fit and calculate AIC and BIC for inverse-transformed model
model_inv <- lm(I(1/Price) ~ Year + Mileage + Horsepower, data=cars_data)
aic_inv <- AIC(model_inv)
bic_inv <- BIC(model_inv)

# Print AIC and BIC for the inverse-transformed model
print(paste("Inverse Model AIC:", aic_inv))
print(paste("Inverse Model BIC:", bic_inv))



# Set the seed for reproducibility
set.seed(123)

# Create a random sample of row indices for the training set
train_indices <- sample(1:nrow(cars_data), size = 0.8 * nrow(cars_data))

# Create training and testing sets
training_set <- cars_data[train_indices, ]
testing_set <- cars_data[-train_indices, ]



# Print the dimensions of the training and testing sets to verify the split
cat("Training set dimensions: ", dim(training_set), "\n")
cat("Testing set dimensions: ", dim(testing_set), "\n")

# Optionally, view a summary or the first few rows to inspect the datasets
summary(training_set)
summary(testing_set)

# View the first few rows of each set
head(training_set)
head(testing_set)



# Fit the regression model using inverse-transformed Price
model_train <- lm(Price_inv ~ Year + Mileage + Horsepower, data = training_set)
# Summarize the model to look at coefficients and overall fit
summary(model_train)



# Load necessary library for cross-validation
library(caret)
# Set up cross-validation with 10 folds
train_control <- trainControl(method = "cv", number = 10)
# Train the model with cross-validation
model_cv <- train(Price_inv ~ Year + Mileage + Horsepower, data = training_set, 
                  method = "lm", trControl = train_control)
# Summarize the cross-validation results
print(model_cv)




# Use the model to make predictions on the test set
predicted_prices_inv <- predict(model_train, newdata = testing_set)

# Transform predictions back to original price scale
# For example if using inverse transformation:
predicted_prices <- 1 / predicted_prices_inv

# Calculate error metrics
actual_prices <- testing_set$Price
mae <- mean(abs(predicted_prices - actual_prices))  # Mean Absolute Error
mse <- mean((predicted_prices - actual_prices)^2)  # Mean Squared Error
rmse <- sqrt(mse)  # Root Mean Squared Error

# Print the errors
cat("MAE:", mae, "MSE:", mse, "RMSE:", rmse, "\n")



# Plotting residuals against fitted values to check for patterns
plot(residuals(model_inv) ~ fitted(model_inv), main="Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")  # Adding a horizontal line at zero

# Creating a QQ plot to check for normality of residuals
qqnorm(residuals(model_inv))
qqline(residuals(model_inv), col="red")


# Adding an interaction term example
model_interaction <- lm(Price_inv ~ Year * Mileage + Horsepower, data = training_set)
summary(model_interaction)

# Adding a polynomial term (quadratic term for Mileage)
model_poly <- lm(Price_inv ~ poly(Mileage, 2) + Year + Horsepower, data = training_set)
summary(model_poly)

# Calculate robust standard errors
library(sandwich)
library(lmtest)
robust_se <- coeftest(model_poly, vcov = vcovHC(model_poly, type = "HC1"))
print(robust_se)

# Identifying potential outliers
plot(model_poly, which = 4)  # Plot which shows Cook's distance to identify influential points

# Fitting a non-linear model (e.g., generalized additive models)
library(mgcv)
model_gam <- gam(Price_inv ~ s(Mileage) + s(Horsepower) + Year, data = training_set)
summary(model_gam)

# Using Random Forest for regression
library(randomForest)
set.seed(123)  # for reproducibility
model_rf <- randomForest(Price_inv ~ ., data = training_set)
print(model_rf)

install.packages("randomForest")
# Load the randomForest package
library(randomForest)
# Install the randomForest package if it's not already installed
if (!require("randomForest")) {
  install.packages("randomForest")
  library(randomForest)
} else {
  library(randomForest)
}

#running again
set.seed(123)
model_rf <- randomForest(Price_inv ~ ., data = training_set, importance = TRUE)


# Set a random seed for reproducibility
set.seed(123)

# Train a Random Forest regression model
model_rf <- randomForest(Price_inv ~ ., data = training_set, importance = TRUE)

# Print the model summary
print(model_rf)

# Check variable importance
importance(model_rf)

# Predict on the testing set
predictions_rf <- predict(model_rf, newdata = testing_set)

# Calculate performance metrics
mae_rf <- mean(abs((1 / predictions_rf) - (1 / testing_set$Price_inv)))
mse_rf <- mean(((1 / predictions_rf) - (1 / testing_set$Price_inv))^2)
rmse_rf <- sqrt(mse_rf)

# Print out performance metrics
cat("MAE for Random Forest:", mae_rf, "\n")
cat("MSE for Random Forest:", mse_rf, "\n")
cat("RMSE for Random Forest:", rmse_rf, "\n")


# K-fold cross-validation using caret package
set.seed(123)
fitControl <- trainControl(method = "cv", number = 10)
model_cv <- train(Price_inv ~ ., data = training_set, method = "rf", trControl = fitControl)
print(model_cv)

# Install the caret package if it's not already installed
if (!require("caret")) {
  install.packages("caret", dependencies=TRUE)
  library(caret)
} else {
  library(caret)
}

# running cross-validation setup again
set.seed(123)
fitControl <- trainControl(method = "cv", number = 10)
model_cv <- train(Price_inv ~ ., data = training_set, method = "rf", trControl = fitControl)
print(model_cv)



# Predictions on the testing set using the refined model
predicted_prices_inv <- predict(model_rf, newdata = testing_set)
predicted_prices <- 1 / predicted_prices_inv  # used inverse transformation

# Calculate final error metrics
actual_prices <- testing_set$Price
mae <- mean(abs(predicted_prices - actual_prices))
mse <- mean((predicted_prices - actual_prices)^2)
rmse <- sqrt(mse)

# Print the final error metrics
cat("Final MAE:", mae, "Final MSE:", mse, "Final RMSE:", rmse, "\n")



# Create a new data frame for predictions
new_data <- data.frame(
  Year = factor("2023", levels = c("2018", "2019", "2020", "2021", "2022", "2023")),
  Mileage = 7500,
  Horsepower = 250
)

# Predicting with a confidence interval
conf_intervals <- predict(model_inv, newdata = new_data, interval = "confidence")

# Predicting with a prediction interval
pred_intervals <- predict(model_inv, newdata = new_data, interval = "prediction")

# View the confidence intervals
print(conf_intervals)

# View the prediction intervals
print(pred_intervals)



# Convert inverse-transformed intervals back to price scale
actual_price_fit <- 1 / conf_intervals[1, "fit"]
actual_price_conf_lwr <- 1 / conf_intervals[1, "lwr"]
actual_price_conf_upr <- 1 / conf_intervals[1, "upr"]

actual_price_pred_lwr <- 1 / pred_intervals[1, "lwr"]
actual_price_pred_upr <- 1 / pred_intervals[1, "upr"]

# Print the converted intervals
cat("Predicted price:", actual_price_fit, "\n")
cat("Confidence interval:", actual_price_conf_lwr, "to", actual_price_conf_upr, "\n")
cat("Prediction interval:", actual_price_pred_lwr, "to", actual_price_pred_upr, "\n")








