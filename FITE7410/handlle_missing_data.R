## Load necessary libraries
library(mice)
library(DMwR2)
library(zoo)
library(randForest)
library(lubridate)
library(dplyr)


# Replace the missing value in a specific column with the mean of that column
fill_missing_with_mean <- function(data, column) {
  data[[column]][is.na(data[[column]])] <- mean(data[[column]], na.rm = TRUE)
  return(data)
}

# Replace the missing value in a specific column with the median of that column
fill_missing_with_median <- function(data, column) {
  data[[column]][is.na(data[[column]])] <- median(data[[column]], na.rm = TRUE)
  return(data)
}   

# Replace the missing value in a specific column with the mode of that column
fill_missing_with_mode <- function(data, column) {
  mode_value <- as.numeric(names(sort(table(data[[column]]), decreasing = TRUE)[1]))
  data[[column]][is.na(data[[column]])] <- mode_value
  return(data)
}

# Replace the missing value in a specific column with a value from linear interpolation
fill_missing_with_interpolation <- function(data, column) { 
  data[[column]] <- zoo::na.approx(data[[column]], na.rm = FALSE)
  return(data)
}

# Replace the missing value in a specific column with a value from linear regression
fill_missing_with_regression <- function(data, target_column, predictor_columns) {
  formula <- as.formula(paste(target_column, paste(predictor_columns, collapse = " + "), sep = " ~ "))
  model <- lm(formula, data = data, na.action = na.exclude)
  missing_indices <- which(is.na(data[[target_column]]))
  predicted_values <- predict(model, newdata = data[missing_indices, ])
  data[[target_column]][missing_indices] <- predicted_values
  return(data)
}

# Replace the missing value in a specific column using multiple imputation
fill_missing_with_mice <- function(data, m = 5, maxit = 50 ) {
  imputed_data <- mice(data, m = m, maxit = maxit, method = 'pmm', seed = 500)
  completed_data <- complete(imputed_data, 1)
  return(completed_data)
} 

# replace the missing value in a specific column using k-nearest neighbors
fill_missing_with_knn <- function(data, k = 5) {
  imputed_data <- DMwR2::knnImputation(data, k = k)
  return(imputed_data)
}   

# replace the missing value in a specific column using random forest
fill_missing_with_random_forest <- function(data, target_column, predictor_columns) {
  formula <- as.formula(paste(target_column, paste(predictor_columns, collapse = " + "), sep = " ~ "))
  model <- randomForest(formula, data = data, na.action = na.exclude)
  missing_indices <- which(is.na(data[[target_column]]))
  predicted_values <- predict(model, newdata = data[missing_indices, ])
  data[[target_column]][missing_indices] <- predicted_values
  return(data)
}

# Replace the missing value in a specific column with the last observation carried forward
fill_missing_with_locf <- function(data, column) {  
  data[[column]] <- zoo::na.locf(data[[column]], na.rm = FALSE)
  return(data)
}   