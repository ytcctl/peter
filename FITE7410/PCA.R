# Principal Component Analysis (PCA) to reduce dimensionality of numeric data
# and visualize the results.
library(FactoMineR)
library(factoextra) # for PCA visualization (install if not already installed)
# optional: for missing-value PCA
library(pcaMethods) # for PCA with missing values (install if not already installed)    

# Function to perform PCA and visualize results
perform_pca <- function(data, scale_data = TRUE, ncp = 5) {
  # Select only numeric columns
  numeric_data <- data %>% select(where(is.numeric))
    
  # Perform PCA
  pca_result <- PCA(numeric_data, scale.unit = scale_data, ncp = ncp, graph = FALSE)

  # Visualize PCA results
  fviz_pca_ind(pca_result, geom = "point", pointshape = 21, pointsize = 2,
               fill = "lightblue", col.ind = "black", repel = TRUE) +
    labs(title = "PCA - Individuals") +
    theme_minimal()

  fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("blue", "white", "red"),
               repel = TRUE) +
    labs(title = "PCA - Variables") +
    theme_minimal()
}

# Example usage:
# perform_pca(mtcars)  # Replace mtcars with your dataset   
# For datasets with missing values, consider using pcaMethods package
# Example for missing-value PCA:
# pca_missing <- pca(numeric_data, method = "svdImpute", nPcs = 5)
# summary(pca_missing)
# plot(pca_missing) # Scree plot for missing-value PCA
# To visualize missing data patterns, you can use the following function
visualize_missing_data <- function(data) {
  library(Amelia)
  missmap(data, main = "Missing Data Map", col = c("red", "green"), legend = TRUE)
}   

# Function to visualize the percentage distribution of a categorical variable
visualize_categorical_percentage <- function(data, column) {
  perc_data <- data %>%
    group_by(across(all_of(column))) %>%
    summarise(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100)
  ggplot(perc_data, aes_string(x = column, y = "percentage")) +
    geom_bar(stat = "identity", fill = "lightcoral") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vertical = -0.5) +
    labs(title = paste("Percentage Distribution of", column), x = column, y = "Percentage") +
    theme_minimal()
}
# Example usage:
# visualize_categorical_percentage(mtcars, "cyl")   

# Function to visualize the distribution of a numeric variable
visualize_distribution <- function(data, column) {
  p <- ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = (max(data[[column]], na.rm = TRUE) - min(data[[column]], na.rm = TRUE)) / 30, fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_distribution(mtcars, "mpg")

# Function to scale and center numeric data
scale_numeric_data <- function(data) {
  numeric_data <- data %>% select(where(is.numeric))
  scaled_data <- scale(numeric_data)
  return(as.data.frame(scaled_data))
}
# Example usage:
# scaled_mtcars <- scale_numeric_data(mtcars)

