# This is a sample R script for assignment 1.

# Install packages/load libraries
install.packages('package_name')
library(package_name)
...


# Load the data
dataset = read.csv("file_path")


# Exploratory Data Analysis

# 1. Distinguish Attributes
str(dataset)
summary(dataset)
...


# 2. Univariate Analysis
ggplot(...)
hist(...)
...


# 3. Bi-/Multi-variate Analysis
ggplot(dataset, aes(...)) + geom_bar()
corrplot(...)
......


