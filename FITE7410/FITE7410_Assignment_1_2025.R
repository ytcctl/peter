# FITE7410 Financial Fraud Analytics First Semester, 2025-2026 -----------------
# Assignnment 1 Exploratory Data Analysis (EDA)
# Student: Choi Chi For 3036384024
# Due Date: 19 Oct, 2025 (Sun) 23:59
# Objective of this assignment:
# 1. Perform data cleaning and preparation
# 2. Explore and visualize the data to identify patterns and trends
# 3. Engineer new features based on domain knowledge or insights from EDA
# 4. Prepare a report summarizing the findings from EDA


# Install essential packages
install.packages(dplyr)
install.packages(tidyr)
install.packages(ggplot2)
install.packages(corrplot)
install.packages(stringr)

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)

#  Load data 
tryCatch({
  data <- data.frame(read.csv("A1_data.csv"))
}, warning = function(w) {
  
  print(w)
}, error = function(e) {
  print(e)
}, finally = {
print("Data loading attempt finished.")
})

# Display the first 20 rows of the dataset
head(data, n = 20)

#  View summary statistics of the dataset 
summary(data)

# view the unique values in each column 
unique_values <- sapply(data, function(x) length(unique(x)))
unique_values <- data.frame(Variable = names(unique_values), Unique_Values = unique_values)
print(unique_values)

# view the percentage of isFraud in the dataset 
table(data$isFraud) / nrow(data) * 100

# visualize the distribution of isFraud using the pie chart with legend and figure inside the pie charts ------------------------------
fraud_counts <- table(data$isFraud)
fraud_labels <- c("Non Fraud", "Fraud")
fraud_colors <- c("lightblue", "salmon")
fraud_percentages <- round(fraud_counts / sum(fraud_counts) * 100, 1)
fraud_labels <- paste(fraud_labels, "\n", fraud_percentages, "%", sep = "")
pie(fraud_counts, labels = fraud_labels, col = fraud_colors, main = "Distribution of Fraudulent Transactions")
legend("topright", legend = c("Not Fraud", "Fraud"), fill = fraud_colors)

# View the percentage of missing values in column with numeric value ------------------------------
numeric_cols <- sapply(data, is.numeric)
missing_percentage <- sapply(data[, numeric_cols], function(x) {
  sum(is.na(x)) / length(x) * 100
})
missing_percentage <- data.frame(missing_percentage[missing_percentage > 0])

# View the percentage of the value = "" in the column with character value ------------------------------
char_cols <- sapply(data, is.character)
empty_string_percentage <- sapply(data[, char_cols], function(x) {
  sum(x == "", na.rm = TRUE) / length(x) * 100
})
empty_string_percentage <- data.frame(empty_string_percentage[empty_string_percentage > 0])


missing_data_summary <- data.frame()


if(nrow(missing_percentage) > 0) {
  missing_df <- data.frame(
    Metric = "Missing Values (%)",
    Column = rownames(missing_percentage),
    Percentage = missing_percentage[,1]
  )
  missing_data_summary <- rbind(missing_data_summary, missing_df)
}


if(nrow(empty_string_percentage) > 0) {
  empty_df <- data.frame(
    Metric = "Empty Strings (%)",
    Column = rownames(empty_string_percentage),
    Percentage = empty_string_percentage[,1]
  )
  missing_data_summary <- rbind(missing_data_summary, empty_df)
}

rownames(missing_data_summary) <- NULL
print(missing_data_summary)

# Handle Missing data: Select the column with the percentage of missing values is less than 50% ------------------------------
cols_to_keep <- setdiff(names(data), missing_data_summary$Column[missing_data_summary$Percentage > 50])
data <- data[, cols_to_keep]


# Handle Missing data: transform the data in columns with character value == "" to "unknown" ------------------------------
data[data == ""] <- "unknown"
data

# view the distribution of TxnAmt group by isFraud == 0  ------------------------------
ggplot(data %>% filter(isFraud == 0), aes(x = TxnAmt)) +
  geom_histogram(binwidth = 100, fill="green", color = "dark green", alpha = 0.7) +
  labs(title = "Distribution of Transaction Amount (TxnAmt) for Non-Fraudulent Transactions", x = "Transaction Amount", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(data$TxnAmt, na.rm = TRUE), by = 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # view the distribution of TxnAmt group by isFraud == 1 ------------------------------
ggplot(data %>% filter(isFraud == 1), aes(x = TxnAmt)) +
  geom_histogram(binwidth = 100, fill="red", color = "dark red", alpha = 0.7) +
  labs(title = "Distribution of Transaction Amount (TxnAmt) for Fraudulent Transactions", x = "Transaction Amount", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(data$TxnAmt, na.rm = TRUE), by = 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# view the log transformation of TxnAmt group by isFraud ------------------------------
data <- data %>% mutate(Log_TxnAmt = log(TxnAmt + 1))  # Adding 1 to avoid log(0) 

ggplot(data, aes(x = Log_TxnAmt, fill = factor(isFraud))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Log Transformed TxnAmt by isFraud",
       x = "Log Transformed Transaction Amount", 
       y = "Density",
       fill = "isFraud") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(data$Log_TxnAmt, na.rm = TRUE), by = 0.5)) +     
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  scale_y_continuous(breaks = seq(0, max(ggplot_build(ggplot(data, aes(x = Log_TxnAmt, fill = factor(isFraud))) +
  geom_density(alpha = 0.5))$data[[1]]$density, na.rm = TRUE), by = 0.05))


  # view the percentage of TxnAmt with interval range of 100 ------------------------------
original_data <- data

data <- data %>%
  mutate(TxnAmt_Bin = case_when(
    TxnAmt <= 100 ~ "0-100",
    TxnAmt <= 200 ~ "100-200",
    TxnAmt <= 300 ~ "200-300",
    TxnAmt <= 400 ~ "300-400",
    TxnAmt <= 500 ~ "400-500",
    TxnAmt <= 600 ~ "500-600",
    TxnAmt <= 700 ~ "600-700",
    TxnAmt <= 800 ~ "700-800",
    TxnAmt <= 900 ~ "800-900",
    TxnAmt <= 1000 ~ "900-1000",
    TRUE ~ "Above 1000"
  )) %>%
  group_by(TxnAmt_Bin) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


ggplot(data, aes(x = TxnAmt_Bin, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Percentage of Transaction Amount (TxnAmt)", x = "Transaction Amount Bins", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data$Percentage, na.rm = TRUE), by = 5))

  # Restore original data for next analysis
data <- original_data

# visualize the percentage of TxnAmt with interval range of 100 group by isFraud and non-fraud ------------------------------
original_data <- data
data <- data %>%
  mutate(TxnAmt_Bin = case_when(
    TxnAmt <= 100 ~ "0-100",
    TxnAmt <= 200 ~ "100-200",
    TxnAmt <= 300 ~ "200-300",
    TxnAmt <= 400 ~ "300-400",
    TxnAmt <= 500 ~ "400-500",
    TxnAmt <= 600 ~ "500-600",
    TxnAmt <= 700 ~ "600-700",
    TxnAmt <= 800 ~ "700-800",
    TxnAmt <= 900 ~ "800-900",
    TxnAmt <= 1000 ~ "900-1000",
    TRUE ~ "Above 1000"
  )) %>%
  group_by(TxnAmt_Bin, isFraud) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


ggplot(data, aes(x = TxnAmt_Bin, y = Percentage, fill = factor(isFraud))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Transaction Amount (TxnAmt) isFraud", 
       x = "Transaction Amount Bins", 
       y = "Percentage",
       fill = "isFraud") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data$Percentage, na.rm = TRUE), by = 5))

  # Restore original data for next analysis
data <- original_data


# Feature Engineering: Convert TxnDT to DateTime format and extract useful features ------------------------------
data <- data %>% mutate(TxnDT = as.POSIXct(TxnDT, origin = "1970-01-01", tz = "UTC"))
data <- data %>% mutate(TransactionHour = as.numeric(format(TxnDT, "%H")),
                        TransactionDay = as.numeric(format(TxnDT, "%d")),
                        TransactionMonth = as.numeric(format(TxnDT, "%m")),
                        TransactionWeekday = as.numeric(format(TxnDT, "%u"))) 

# count the number of transactions per hour ------------------------------
transactions_per_hour <- data %>% group_by(TransactionHour) %>%
  summarise(Count = n())

# count the number of isFraud and non-fraud per hour ------------------------------
fraud_per_hour <- data %>% group_by(TransactionHour) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud))

# visualize the number of isFraud and non-fraud per hour ------------------------------
ggplot(fraud_per_hour, aes(x = TransactionHour)) +
  geom_line(aes(y = FraudCount, color = "Fraudulent Transactions"), size = 1) +
  geom_line(aes(y = NonFraudCount, color = "Non-Fraudulent Transactions"), size = 1) +
  labs(title = "Number of Fraudulent and Non-Fraudulent Transactions per Hour",
       x = "Hour of Day",
       y = "Number of Transactions",
       color = "Transaction Type") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(breaks = seq(0, max(fraud_per_hour$NonFraudCount, na.rm = TRUE), by = 5000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # view the percentage of isFraud and non-fraud by TransactionHour ------------------------------
fraud_non_fraud_percentage_per_hour <- fraud_per_hour %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100)

ggplot(fraud_non_fraud_percentage_per_hour, aes(x = TransactionHour)) +
  geom_line(aes(y = FraudPercentage, color = "Fraud"), size = 1) +
  geom_line(aes(y = NonFraudPercentage, color = "Non-Fraud"), size = 1) +
  labs(title = "Percentage of Fraudulent and Non-Fraudulent Transactions by Hour",
       x = "Hour of Day",
       y = "Percentage of Transactions (%)") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue"), name = "Transaction Type")


  # view the percentage of transactions per month group by isFraud and Non-Fraud ------------------------------
fraud_non_fraud_percentage_per_month <- transactions_per_month %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100)

ggplot(fraud_non_fraud_percentage_per_month, aes(x = TransactionMonth)) +
  geom_line(aes(y = FraudPercentage, color = "Fraud"), size = 1) +
  geom_line(aes(y = NonFraudPercentage, color = "Non-Fraud"), size = 1) +
  labs(title = "Percentage of Fraudulent and Non-Fraudulent Transactions by Month",
       x = "Month",
       y = "Percentage of Transactions (%)") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue"), name = "Transaction Type")


  # view the distribution of card4 group by isFraud ------------------------------
ggplot(data, aes(x = card4, fill = factor(isFraud))) +
  geom_bar() +
  labs(title = "Distribution of card4 by isFraud",
       x = "card4",
       y = "Count",
       fill = "isFraud") +
  theme_minimal()


# view the percentage of card4 group by isFraud ------------------------------
  group_by(card4) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(card4_fraud_percentage)

# visualize the percentage of card4 group by isFraud ------------------------------

card4_plot_data <- card4_fraud_percentage %>%
  select(card4, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(card4_plot_data, aes(x = card4, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of card4 by isFraud",
       x = "card4",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of card6 group by isFraud ------------------------------
card6_fraud_percentage <- data %>%
  group_by(card6) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(card6_fraud_percentage)

# visualize the percentage of card6 group by isFraud ------------------------------
card6_plot_data <- card6_fraud_percentage %>%
  select(card6, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  )) 

ggplot(card6_plot_data, aes(x = card6, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of card6 by isFraud",
       x = "card6",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of DevType group by isFraud ------------------------------
devtype_fraud_percentage <- data %>%
  group_by(DevType) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(devtype_fraud_percentage)


devtype_plot_data <- devtype_fraud_percentage %>%
  select(DevType, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(devtype_plot_data, aes(x = DevType, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of DevType by isFraud",
       x = "DevType",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")


# view the percentage of ProductCD group by isFraud ------------------------------
productcd_fraud_percentage <- data %>%
  group_by(ProductCD) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(productcd_fraud_percentage)   


productcd_plot_data <- productcd_fraud_percentage %>%
  select(ProductCD, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(productcd_plot_data, aes(x = ProductCD, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of ProductCD by isFraud",
       x = "ProductCD",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")   

# view the percentage of isFraud by addr1 which is.na and not is.na ------------------------------------------------
addr1_fraud_summary <- data %>%
  mutate(addr1_is_na = ifelse(is.na(addr1), "NA", "Not NA")) %>%
  group_by(addr1_is_na) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(addr1_fraud_summary)

# visualize the percentage of isFraud by addr1 which is.na and not is.na ------------------------------------------------
addr1_plot_data <- addr1_fraud_summary %>%
  select(addr1_is_na, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(addr1_plot_data, aes(x = addr1_is_na, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Fraud" = "red", "Non-Fraud" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of isFraud by addr1 NA Status",
       x = "addr1 NA Status",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")


# find the card1 with the record of isFraud == 1 ------------------------------------------------
fraudulent_card1 <- unique(data$card1[data$isFraud == 1])

# view the percentage of isFraud per card1 which is in fraudulent_card1 ------------------------------------------------
card1_fraud_percentage <- data %>%
  filter(card1 %in% fraudulent_card1) %>%
  group_by(card1) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100) %>%
  arrange(desc(FraudPercentage))

 # view the average percentage of isFraud per card1 which is in fraudulent_card1 ------------------------------------------------
card1_fraud_percentage <- card1_fraud_percentage %>%
  summarise(Average_FraudPercentage = mean(FraudPercentage),
            Average_NonFraudPercentage = mean(NonFraudPercentage))
print(card1_fraud_percentage)


# view the correlation matrix between column from C1 to C14 vs isFraud ------------------------------------------------
numeric_c_columns <- paste0("C", 1:14)

correlations_with_fraud <- data %>%
    select(all_of(numeric_c_columns), isFraud) %>%
    cor(use = "complete.obs")
  
fraud_correlations <- correlations_with_fraud[numeric_c_columns, "isFraud"]
  

cat("\nCorrelations between C1-C14 and isFraud:\n")
print(round(fraud_correlations, 4))
  
ggplot(fraud_corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df$Correlation > 0, "red", "blue")) +
    coord_flip() +
    labs(title = "Correlation of C1-C14 Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)


# view the correlation matrix between column from C1 to C14 vs isFraud ------------------------------------------------
numeric_c_columns <- paste0("C", 1:14)


correlations_with_fraud <- data %>%
    select(all_of(numeric_c_columns), isFraud) %>%
    cor(use = "complete.obs")

fraud_correlations <- correlations_with_fraud[numeric_c_columns, "isFraud"]
  
  # Display correlations
  cat("\nCorrelations between C1-C14 and isFraud:\n")
  print(round(fraud_correlations, 4))
  

ggplot(fraud_corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df$Correlation > 0, "red", "blue")) +
    coord_flip() +
    labs(title = "Correlation of C1-C14 Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)


# view the correlation matrix between column from D1 to D15 vs isFraud ------------------------------------------------
# First, identify D1-D15 columns that exist and are numeric
numeric_d_columns <- paste0("D", c(1,8,9,11))

  # Calculate correlation between each C column and isFraud
  correlations_with_fraud <- data %>%
    select(all_of(numeric_d_columns), isFraud) %>%
    cor(use = "complete.obs")
  
  # Extract only the correlations with isFraud (last row/column)
  fraud_correlations <- correlations_with_fraud[numeric_d_columns, "isFraud"]
  
  # Display correlations
  cat("\nCorrelations between D1-D15 and isFraud:\n")
  print(round(fraud_correlations, 4))
  
  # Create correlation plot
  corrplot(correlations_with_fraud, method = "circle", type = "upper", 
           tl.col = "black", tl.srt = 45, 
           title = "Correlation Matrix: C1-C14 vs isFraud", 
           mar = c(0,0,1,0))
  
  # Create a bar plot showing correlations with isFraud
  fraud_corr_df <- data.frame(
    Variable = names(fraud_correlations),
    Correlation = as.numeric(fraud_correlations)
  )
  
  library(ggplot2)
  ggplot(fraud_corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df$Correlation > 0, "red", "blue")) +
    coord_flip() +
    labs(title = "Correlation of C1-C14 Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)
  
