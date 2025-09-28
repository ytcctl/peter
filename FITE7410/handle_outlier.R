#Apply interquartile range (IQR) method to identify outliers in a numeric vector
identify_outliers_iqr <- function(data) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- which(data < lower_bound | data > upper_bound)
  return(outliers)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# outliers_iqr <- identify_outliers_iqr(numeric_vector)
# cleaned_vector_iqr <- remove_outliers(numeric_vector, outliers_iqr)
# print(cleaned_vector_iqr) 

#Apply Z-score method to identify outliers in a numeric vector
identify_outliers_zscore <- function(data, threshold = 3) {
  z_scores <- (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)
  outliers <- which(abs(z_scores) > threshold)
  return(outliers)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# outliers_zscore <- identify_outliers_zscore(numeric_vector)
# cleaned_vector_zscore <- remove_outliers(numeric_vector, outliers_zscore)
# print(cleaned_vector_zscore)

#Apply Modified Z-score method to identify outliers in a numeric vector
identify_outliers_modified_zscore <- function(data, threshold = 3.5) {
  median_data <- median(data, na.rm = TRUE)
  mad_data <- mad(data, na.rm = TRUE)
  modified_z_scores <- 0.6745 * (data - median_data) / mad_data
  outliers <- which(abs(modified_z_scores) > threshold)
  return(outliers)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# outliers_modified_zscore <- identify_outliers_modified_zscore(numeric_vector)
# cleaned_vector_modified_zscore <- remove_outliers(numeric_vector, outliers_modified_zscore)
# print(cleaned_vector_modified_zscore)

#Apply Percentile method to identify outliers in a numeric vector
identify_outliers_percentile <- function(data, lower_percentile = 0.01, upper_percentile = 0.99) {
  lower_bound <- quantile(data, lower_percentile, na.rm = TRUE)
  upper_bound <- quantile(data, upper_percentile, na.rm = TRUE)
  outliers <- which(data < lower_bound | data > upper_bound)
  return(outliers)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# outliers_percentile <- identify_outliers_percentile(numeric_vector)
# cleaned_vector_percentile <- remove_outliers(numeric_vector, outliers_percentile)
# print(cleaned_vector_percentile)

#Apply Mahalanobis Distance method to identify outliers in a numeric matrix or data frame
identify_outliers_mahalanobis <- function(data, threshold = 0.01) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Input data must be a matrix or data frame.")
  }
  cov_matrix <- cov(data, use = "pairwise.complete.obs")
  center <- colMeans(data, na.rm = TRUE)
  mahalanobis_distances <- mahalanobis(data, center, cov_matrix)
  p_values <- pchisq(mahalanobis_distances, df = ncol(data), lower.tail = FALSE)
  outliers <- which(p_values < threshold)
  return(outliers)
}
# Example usage:
# data_matrix <- matrix(c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11), nrow = 5, ncol = 2) 
# outliers_mahalanobis <- identify_outliers_mahalanobis(data_matrix)
# cleaned_data_mahalanobis <- data_matrix[-outliers_mahalanobis, ]
# print(cleaned_data_mahalanobis)

# Apply Grubb's Test to identify outliers in a numeric vector
identify_outliers_grubbs <- function(data, alpha = 0.05) {
  library(outliers)
  outliers <- c()
  repeat {
    test_result <- grubbs.test(data)
    p_value <- test_result$p.value
    if (p_value < alpha) {
      outlier_value <- as.numeric(strsplit(test_result$alternative, " ")[[1]][3])
      outlier_index <- which(data == outlier_value)[1]
      outliers <- c(outliers, outlier_index)
      data <- data[-outlier_index]
    } else {
      break
    }
  }
  return(outliers)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# outliers_grubbs <- identify_outliers_grubbs(numeric_vector)
# cleaned_vector_grubbs <- remove_outliers(numeric_vector, outliers_grubbs)
# print(cleaned_vector_grubbs)

# Remove outliers from a numeric vector based on identified indices
remove_outliers <- function(data, outlier_indices) {
  cleaned_data <- data[-outlier_indices]
  return(cleaned_data)
}   
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# outliers_iqr <- identify_outliers_iqr(numeric_vector)
# cleaned_vector_iqr <- remove_outliers(numeric_vector, outliers_iqr)
# print(cleaned_vector_iqr)  

# Transform outliers in a numeric vector using Winsorization
winsorize_outliers <- function(data, lower_percentile = 0.01, upper_percentile = 0.99) {
  lower_bound <- quantile(data, lower_percentile, na.rm = TRUE)
  upper_bound <- quantile(data, upper_percentile, na.rm = TRUE)
  data[data < lower_bound] <- lower_bound
  data[data > upper_bound] <- upper_bound
  return(data)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# winsorized_vector <- winsorize_outliers(numeric_vector)
# print(winsorized_vector)

# Transform outliers in a numeric vector using Log Transformation
log_transform_outliers <- function(data) {
  if (any(data <= 0, na.rm = TRUE)) {
    stop("Log transformation is not defined for non-positive values.")
  }
  transformed_data <- log(data)
  return(transformed_data)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# log_transformed_vector <- log_transform_outliers(numeric_vector)
# print(log_transformed_vector)     

# Replace outliers in a numeric vector using Median Absolute Deviation (MAD)
replace_outliers_mad <- function(data, threshold = 3.5) {
  median_data <- median(data, na.rm = TRUE)
  mad_data <- mad(data, na.rm = TRUE)
  modified_z_scores <- 0.6745 * (data - median_data) / mad  _data
  outlier_indices <- which(abs(modified_z_scores) > threshold)
  data[outlier_indices] <- median_data
  return(data)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# replaced_vector_mad <- replace_outliers_mad(numeric_vector)
# print(replaced_vector_mad)

# Replace outliers in a numeric vector using Mean and Standard Deviation
replace_outliers_mean_sd <- function(data, threshold = 3) {
  mean_data <- mean(data, na.rm = TRUE)
  sd_data <- sd(data, na.rm = TRUE)
  outlier_indices <- which(abs(data - mean_data) > threshold * sd_data)
  data[outlier_indices] <- mean_data
  return(data)
}
# Example usage:
# numeric_vector <- c(10, 12, 12, 13, 12, 14, 13, 100, 12, 11)
# replaced_vector_mean_sd <- replace_outliers_mean_sd(numeric_vector)
# print(replaced_vector_mean_sd)