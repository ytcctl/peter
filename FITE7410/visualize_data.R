library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(ggcorrplot)
library(ggpubr)
library(plotly)

# Function to visualize the distribution of a numeric variable
visualize_distribution <- function(data, column) {
  p <- ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = (max(data[[column]], na.rm = TRUE) - min(data[[column]], na.rm = TRUE)) / 30, fill = "blue", color = "black", alpha = 0.7) +
    geom_density(aes_string(y = "..count.. * (max(data[[column]], na.rm = TRUE) - min(data[[column]], na.rm = TRUE)) / 30"), color = "red", size = 1) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_distribution(mtcars, "mpg")


# Function to visualize the relationship between two numeric variables
visualize_relationship <- function(data, column1, column2) {
  p <- ggplot(data, aes_string(x = column1, y = column2)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = paste("Relationship between", column1, "and", column2), x = column1, y = column2) +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_relationship(mtcars, "mpg", "wt")

#Function to visuallize correlation matrix of numeric variables
visualize_correlation <- function(data) {
  numeric_data <- data %>% select(where(is.numeric))
  corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  p <- ggcorrplot(corr_matrix, lab = TRUE, lab_size = 3, method = "circle", type = "lower", 
                  title = "Correlation Matrix", ggtheme = theme_minimal)
  print(p)
}
# Example usage:
# visualize_correlation(mtcars)

# Function to visualize missing data pattern
visualize_missing_data <- function(data) {
  missing_data <- as.data.frame(is.na(data))
  missing_summary <- colSums(missing_data)
  missing_df <- data.frame(Variable = names(missing_summary), MissingCount = missing_summary)
  p <- ggplot(missing_df, aes(x = reorder(Variable, -MissingCount), y = MissingCount)) +
    geom_bar(stat = "identity", fill = "orange") +
    coord_flip() +
    labs(title = "Missing Data Pattern", x = "Variables", y = "Count of Missing Values") +
    theme_minimal()
  print(p)
}

# Function to visualize categorical variable distribution
visualize_categorical_distribution <- function(data, column) {
  p <- ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "purple", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme_minimal()
  print(p)
}

# Function to visualize boxplot of a numeric variable grouped by a categorical variable
visualize_boxplot <- function(data, numeric_column, categorical_column) {
  p <- ggplot(data, aes_string(x = categorical_column, y = numeric_column)) +
    geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot of", numeric_column, "by", categorical_column), x = categorical_column, y = numeric_column) +
    theme_minimal()
  print(p)
}

# Function to visualize pairwise relationships in a dataset
visualize_pairwise_relationships <- function(data) {
  numeric_data <- data %>% select(where(is.numeric))
  p <- ggpairs(numeric_data, title = "Pairwise Relationships")
  print(p)
}
# Example usage:
# visualize_pairwise_relationships(mtcars)  

# Function to visualize time series data
visualize_time_series <- function(data, time_column, value_column) {
  p <- ggplot(data, aes_string(x = time_column, y = value_column)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(title = paste("Time Series of", value_column), x = time_column, y = value_column) +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_time_series(airquality, "Day", "Temp")  

# Function to visualize outliers in a numeric variable using boxplot
visualize_outliers <- function(data, column) {
  p <- ggplot(data, aes_string(y = column)) +
    geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot of", column, "to Visualize Outliers"), y = column) +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_outliers(mtcars, "mpg")

# Function to visualize relationship between two numeric variables with scatter plot and density plots
visualize_scatter_with_density <- function(data, column1, column2) {
  p1 <- ggplot(data, aes_string(x = column1, y = column2)) +
    geom_point(alpha = 0.6) + 
    geom_density_2d(color = "red") +
    labs(title = paste("Scatter Plot with Density of", column1, "and", column2), x = column1, y = column2) +
    theme_minimal()
  print(p1)
}
# Example usage:
# visualize_scatter_with_density(mtcars, "mpg", "wt")  

# Function to visualize multiple numeric variables using faceted histograms
visualize_faceted_histograms <- function(data, columns) {
  melted_data <- melt(data, measure.vars = columns)
  p <- ggplot(melted_data, aes(x = value)) +
    geom_histogram(binwidth = (max(melted_data$value, na.rm = TRUE) - min(melted_data$value, na.rm = TRUE)) / 30, fill = "cyan", color = "black", alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Faceted Histograms of Numeric Variables", x = "Value", y = "Count") +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_faceted_histograms(mtcars, c("mpg", "wt", "hp"))    

# Function to visualize categorical variable distribution with percentages
visualize_categorical_percentage <- function(data, column) {
  total_count <- nrow(data)
  percentage_data <- data %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / total_count) * 100)
  p <- ggplot(percentage_data, aes_string(x = column, y = "Percentage")) +
    geom_bar(stat = "identity", fill = "lightcoral") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), vertical = -0.5) +
    labs(title = paste("Percentage Distribution of", column), x = column, y = "Percentage") +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_categorical_percentage(mtcars, "cyl")

# Function to visualize correlation between numeric variables using heatmap
visualize_correlation_heatmap <- function(data) {
  numeric_data <- data %>% select(where(is.numeric))
  corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  melted_corr <- melt(corr_matrix)
  p <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1, 1), name = "Correlation") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_correlation_heatmap(mtcars)

# Function to visualize multiple numeric variables using density plots
visualize_density_plots <- function(data, columns) {
  melted_data <- melt(data, measure.vars = columns)
  p <- ggplot(melted_data, aes(x = value, fill = variable)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plots of Numeric Variables", x = "Value", y = "Density") +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_density_plots(mtcars, c("mpg", "wt", "hp"))

# Function to visualize grouped boxplots of a numeric variable by a categorical variable
visualize_grouped_boxplots <- function(data, numeric_column, categorical_column) {
  p <- ggplot(data, aes_string(x = categorical_column, y = numeric_column, fill = categorical_column)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplots of", numeric_column, "by", categorical_column), x = categorical_column, y = numeric_column) +
    theme_minimal() +
    theme(legend.position = "none")
  print(p)
}
# Example usage:
# visualize_grouped_boxplots(mtcars, "mpg", "cyl")
# Function to visualize time series data with smoothing
visualize_time_series_with_smoothing <- function(data, time_column, value_column) {
  p <- ggplot(data, aes_string(x = time_column, y = value_column)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    geom_smooth(method = "loess", color = "green", se = FALSE) +
    labs(title = paste("Time Series of", value_column, "with Smoothing"), x = time_column, y = value_column) +
    theme_minimal()
  print(p)
}
# Example usage:
# visualize_time_series_with_smoothing(airquality, "Day", "Temp")

# Function to visualize multiple numeric variables using scatter plot matrix
visualize_scatter_plot_matrix <- function(data) {
  numeric_data <- data %>% select(where(is.numeric))
  p <- ggpairs(numeric_data, title = "Scatter Plot Matrix of Numeric Variables")
  print(p)
}

# Example usage:
# visualize_scatter_plot_matrix(mtcars) 

# Function to visualize interactive scatter plot using plotly
visualize_interactive_scatter <- function(data, column1, column2) {
  p <- plot_ly(data, x = ~get(column1), y = ~get(column2), type = 'scatter', mode = 'markers',
               marker = list(size = 10, color = 'rgba(152, 0, 0, .8)', line = list(width = 2, color = 'rgb(0, 0, 0)'))) %>%
    layout(title = paste("Interactive Scatter Plot of", column1, "and", column2),
           xaxis = list(title = column1),
           yaxis = list(title = column2))
  print(p)
}
# Example usage:
# visualize_interactive_scatter(mtcars, "mpg", "wt")

