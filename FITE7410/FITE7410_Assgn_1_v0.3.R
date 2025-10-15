# FITE7410 Financial Fraud Analytics First Semester, 2025-2026 -----------------
# Assignment 1 Exploratory Data Analysis (EDA)
# Student: Choi Chi For 3036384024
# Due Date: 19 Oct, 2025 (Sun) 23:59
# Objective of this assignment:
# 1. Perform data cleaning and preparation
# 2. Explore and visualize the data to identify patterns and trends
# 3. Engineer new features based on domain knowledge or insights from EDA
# 4. Prepare a report summarizing the findings from EDA

# install necessary libraries ----------
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("corrplot")
install.packages("stringr")

# load necessary libraries ----------
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(stringr)

#  Load data ----------
tryCatch({
  data <- data.frame(read.csv("A1_data.csv"))
}, warning = function(w) {
  
  print(w)
}, error = function(e) {
  print(e)
}, finally = {
print("Data loading attempt finished.")
})

head(data)

#  View summary statistics of the dataset ----------
summary(data)

# View the percentage of missing values in column with numeric value ----------
numeric_cols <- sapply(data, is.numeric)
missing_percentage <- sapply(data[, numeric_cols], function(x) {
  sum(is.na(x)) / length(x) * 100
})
missing_percentage <- data.frame(missing_percentage[missing_percentage > 0])

# View the percentage of the value = "" in the column with character value ----------
char_cols <- sapply(data, is.character)
empty_string_percentage <- sapply(data[, char_cols], function(x) {
  sum(x == "", na.rm = TRUE) / length(x) * 100
})
empty_string_percentage <- data.frame(empty_string_percentage[empty_string_percentage > 0])

# Ensure both data frames have the same column structure ----------
missing_data_summary <- data.frame()

# Add missing values data if any exist ----------
if(nrow(missing_percentage) > 0) {
  missing_df <- data.frame(
    Metric = "Missing Values (%)",
    Column = rownames(missing_percentage),
    Percentage = missing_percentage[,1]
  )
  missing_data_summary <- rbind(missing_data_summary, missing_df)
}

# Add empty strings data if any exist ----------
if(nrow(empty_string_percentage) > 0) {
  empty_df <- data.frame(
    Metric = "Empty Strings (%)",
    Column = rownames(empty_string_percentage),
    Percentage = empty_string_percentage[,1]
  )
  missing_data_summary <- rbind(missing_data_summary, empty_df)
}

# Set proper row names ----------
rownames(missing_data_summary) <- NULL
print(missing_data_summary)

# Select the column with the percentage of missing values is less than 50% ----------
cols_to_keep <- setdiff(names(data), missing_data_summary$Column[missing_data_summary$Percentage > 50])
data <- data.frame(data[, cols_to_keep])

# get the column names of data after removing columns with more than 50% missing values ----------
colnames(data)

# view the unique values in each column ----------
unique_values <- sapply(data, function(x) length(unique(x)))
unique_values <- data.frame(Variable = names(unique_values), Unique_Values = unique_values)
print(unique_values)

# view the unique value from columns M1 - M9 ----------
m_cols <- grep("^M[1-9]$", names(data), value = TRUE)
m_unique_values <- sapply(data[m_cols], function(x) unique(x))
m_unique_values <- data.frame(Variable = names(m_unique_values), Unique_Values = I(m_unique_values))
print(m_unique_values)

# Drop the column of M1 - M9 ----------
data <- data %>% select(-all_of(m_cols))

# transform the data in columns with character value == "" to "unknown" ----------
data[data == ""] <- "Empty Value"

# Convert TxnDT to DateTime format and extract useful features ----------
data <- data %>% mutate(TxnDT = as.POSIXct(TxnDT, origin = "1970-01-01", tz = "UTC"))
data <- data %>% mutate(TransactionHour = as.numeric(format(TxnDT, "%H")),
                        TransactionDay = as.numeric(format(TxnDT, "%d")),
                        TransactionMonth = as.numeric(format(TxnDT, "%m")),
                        TransactionWeekday = as.numeric(format(TxnDT, "%u")))  # 1 = Monday, 7 = Sunday   

# view the number of transactions by isFraud and non-isFraud ----------
data_fraud <- data %>%
  group_by(isFraud) %>%
  summarise(Total_Transactions = n())

data_fraud

# view the percentage of isFraud in the dataset ----------
table(data$isFraud) / nrow(data) * 100

# visualize the distribution of isFraud using the pie chart inluding the table ----------
fraud_counts <- table(data$isFraud)
fraud_labels <- c("Not Fraud", "Fraud")
fraud_colors <- c("lightblue", "salmon")
fraud_percentages <- round(fraud_counts / sum(fraud_counts) * 100, 1)
fraud_labels <- paste(fraud_labels, "\n", fraud_percentages, "%", sep = "")
pie(fraud_counts, labels = fraud_labels, col = fraud_colors, main = "Distribution of Fraudulent Transactions")
legend("topright", legend = c("Not Fraud", "Fraud"), fill = fraud_colors)   

# view the distribution of TxnAmt group by isFraud == 0 and isFraud == 1 ----------
ggplot(data, aes(x = TxnAmt, fill = factor(isFraud))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of TxnAmt by isFraud",
       x = "Transaction Amount",
       y = "Count",
       fill = "isFraud") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(data$TxnAmt, na.rm = TRUE), by = 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# view the distribution of TxnAmt group by isFraud using boxplots ----------

boxplot(TxnAmt ~ isFraud, data = data, main = "Transaction Amount by Fraud Status", 
        xlab = "isFraud", ylab = "Transaction Amount", ylim=c(0, max(data$TxnAmt, na.rm=TRUE) + 100), yaxt="n")
axis(2, at=seq(0, max(data$TxnAmt, na.rm=TRUE) + 100, by=100), las=2)

# view the log transformation of TxnAmt and y tick size = 0.05 and  x tick size = 0.5 ----------
data <- data %>% mutate(Log_TxnAmt = log(TxnAmt + 1))  

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

# Visualize the percentage of TxnAmt with interval of ----------
# 0-100, 100-200, 200-300, 300-400, 400-500, 500-600, 600-700, ----------
# 700-800, 800-900, 900-1000 and above 1000 where isFraud == 0 and isFraud ==1 ----------
data_fraud <- data %>%
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
  group_by(isFraud, TxnAmt_Bin) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

  data_fraud
  
  
# visualize the percentage of TxnAmt with interval of ----------
# 0-100, 100-200, 200-300, 300-400, 400-500, 500-600, ----------
# 600-700, 700-800, 800-900, 900-1000 and above 1000 where isFraud == 0 and isFraud ==1 ----------
ggplot(data_fraud, aes(x = TxnAmt_Bin, y = Percentage, fill = factor(isFraud))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Transaction Amount (TxnAmt) by isFraud", 
       x = "Transaction Amount Bins", 
       y = "Percentage",
       fill = "isFraud") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data_fraud$Percentage, na.rm = TRUE), by = 5))

# view TxnAmt with interval of ----------
# 0-100, 100-200, 200-300, 300-400, 400-500, 500-600, ----------
# 600-700, 700-800, 800-900, 900-1000 and above 1000 group by isFraud and non-fraud ----------
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

# print the data frame to check the values ----------
data

# visualize TxnAmt with interval of ----------
# 0-100, 100-200, 200-300, 300-400, 400-500, 500-600, ----------
# 600-700, 700-800, 800-900, 900-1000 and above 1000 group by isFraud and non-fraud ----------
ggplot(data, aes(x = TxnAmt_Bin, y = Percentage, fill = factor(isFraud))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Transaction Amount (TxnAmt) isFraud", 
       x = "Transaction Amount Bins", 
       y = "Percentage",
       fill = "isFraud") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data$Percentage, na.rm = TRUE), by = 5))

  # Restore original data for next analysis ----------
data <- original_data

# view the precentage of isFraud and non-fraud by TransactionWeekday ----------
data %>%
  group_by(TransactionMonth, isFraud) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ggplot(aes(x = TransactionMonth, y = Percentage, fill = factor(isFraud))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Fraudulent Transactions by Transaction Month",
       x = "Transaction Month", y = "Percentage", fill = "isFraud") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb)

# view the precentage of isFraud and non-fraud by TransactionWeekday ----------
data %>%
  group_by(TransactionWeekday, isFraud) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ggplot(aes(x = TransactionWeekday, y = Percentage, fill = factor(isFraud))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Fraudulent Transactions by Transaction Weekday",
       x = "Transaction Weekday", y = "Percentage", fill = "isFraud") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_continuous(breaks = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# count the number of transactions per hour ----------
transactions_per_hour <- data %>% group_by(TransactionHour) %>%
  summarise(Count = n())

# count the number of isFraud and non-fraud per hour ----------
fraud_per_hour <- data %>% group_by(TransactionHour) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud))

# visualize the number of isFraud and non-fraud per hour with tick-size in y-axis = 200 and x-axis from 0-23 ----------
ggplot(fraud_per_hour, aes(x = TransactionHour)) +
  geom_bar(aes(y = NonFraudCount, fill = "Non-Fraud"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = FraudCount, fill = "Fraud"), stat = "identity", position = "dodge") +
  labs(title = "Number of Fraudulent and Non-Fraudulent Transactions per Hour",
       x = "Transaction Hour",
       y = "Number of Transactions",
       fill = "Transaction Type") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(breaks = seq(0, max(fraud_per_hour$NonFraudCount, fraud_per_hour$FraudCount, na.rm = TRUE), by = 200)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# view the percentage of isFraud and non-fraud by TransactionHour ----------
fraud_non_fraud_percentage_per_hour <- fraud_per_hour %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100)

# visualize the percentage of isFraud and non-fraud by TransactionHour by bar chart ----------
  data %>%
  group_by(TransactionHour, isFraud) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ggplot(aes(x = TransactionHour, y = Percentage, fill = factor(isFraud))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Fraudulent Transactions by Transaction Hour",
       x = "Transaction Hour", y = "Percentage", fill = "isFraud") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_continuous(breaks = 0:23)

# view the count unique value of Card1 where any isFraud == 1 ----------
fraudulent_card1 <- unique(data$card1[data$isFraud == 1])
num_fraudulent_card1 <- length(fraudulent_card1)
print(num_fraudulent_card1)

non_fraudulent_card1 <- unique(data$card1[data$isFraud == 0])
num_non_fraudulent_card1 <- length(non_fraudulent_card1)
print(num_non_fraudulent_card1)

# view the percentage of fraudulent_card1 in pie chart ----------
fraudulent_card1_percentage <- (num_fraudulent_card1 / length(unique(data$card1))) * 100
non_fraudulent_card1_percentage <- 100 - fraudulent_card1_percentage
pie_data <- data.frame(
  Category = c("Fraudulent Card1", "Non-Fraudulent Card1"),
  Percentage = c(fraudulent_card1_percentage, non_fraudulent_card1_percentage)
)
ggplot(pie_data, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Percentage of Fraudulent vs Non-Fraudulent Card1") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) 

# visualize the percentage of total no of record presented by fradulent_card1 ----------
total_records <- nrow(data)
fraudulent_card1_records <- nrow(data[data$card1 %in% fraudulent_card1, ])
fraudulent_card1_record_percentage <- (fraudulent_card1_records / total_records) * 100
non_fraudulent_card1_record_percentage <- 100 - fraudulent_card1_record_percentage
pie_data_records <- data.frame(
  Category = c("Fraudulent Card1 Records", "Non-Fraudulent Card1 Records"),
  Percentage = c(fraudulent_card1_record_percentage, non_fraudulent_card1_record_percentage)
)
ggplot(pie_data_records, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() +
  labs(title = "Percentage of Fraudulent vs Non-Fraudulent Card1 Records") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5)) 

# visualize the cumulative transaction record performed by Fraudulent Card1 over transaction month ----------
fraudulent_card1_monthly <- data %>%
  mutate(Card1Type = ifelse(card1 %in% fraudulent_card1, "Fraudulent Card1", "Non-Fraudulent Card1")) %>%
  group_by(TransactionMonth, Card1Type) %>%
  summarise(MonthlyCount = n()) %>%
  arrange(TransactionMonth) %>%
  group_by(Card1Type) %>%
  mutate(CumulativeCount = cumsum(MonthlyCount))

# Create area plot ----------
ggplot(fraudulent_card1_monthly, aes(x = TransactionMonth, y = CumulativeCount, fill = Card1Type)) +
  geom_area(alpha = 0.6 , position = 'identity') +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Cumulative Transaction Records by Card1 Type Over Months",
       x = "Transaction Month",
       y = "Cumulative Transaction Count",
       fill = "Card1 Type") +
  theme_minimal()

# create a column to indicate the card is fraudulent_card1 ----------
data <- data %>%
  mutate(fraudulent_card1 = ifelse(card1 %in% fraudulent_card1, 1, 0))

# view the monthly no of transaction for each record of card1  ----------
monthly_card1_transactions <- data %>%
  group_by(TransactionMonth, card1) %>%
  summarise(MonthlyCount = n()) %>%
  arrange(TransactionMonth, desc(MonthlyCount))

# add a column in monthly_card1_transactions to indicate the card is fraudulent_card1 in unique value ----------
monthly_card1_transactions <- monthly_card1_transactions %>%
  mutate(fraudulent_card1 = ifelse(card1 %in% unique(fraudulent_card1), 1, 0))

# average the MonthlyCount group by TransactionMonth and fraudulent_card1 ----------
avg_monthly_card1_transactions <- monthly_card1_transactions %>%
  group_by(TransactionMonth, fraudulent_card1) %>%
  summarise(AverageMonthlyCount = mean(MonthlyCount)) %>%
  arrange(TransactionMonth, fraudulent_card1)


# average the avg_monthly_card1_transactions group by fraudulent_card1 ----------
overall_avg_monthly_card1_transactions <- avg_monthly_card1_transactions %>%
  group_by(fraudulent_card1) %>%
  summarise(OverallAverageMonthlyCount = mean(AverageMonthlyCount))
print(overall_avg_monthly_card1_transactions)


# view the count unique value of Card2 where any isFraud == 1 ----------
fraudulent_card2 <- unique(data$card2[data$isFraud == 1])
num_fraudulent_card2 <- length(fraudulent_card2)

# view the percentage of fraudulent_card2 in pie chart ----------
fraudulent_card2_percentage <- (num_fraudulent_card2 / length(unique(data$card2))) * 100
non_fraudulent_card2_percentage <- 100 - fraudulent_card2_percentage
pie_data_card2 <- data.frame(
  Category = c("Fraudulent Card2", "Non-Fraudulent Card2"),
  Percentage = c(fraudulent_card2_percentage, non_fraudulent_card2_percentage)
)
ggplot(pie_data_card2, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Percentage of Fraudulent vs Non-Fraudulent Card2") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5))

# view the count unique value of Card3 where any isFraud == 1 ----------
fraudulent_card3 <- unique(data$card3[data$isFraud == 1])
num_fraudulent_card3 <- length(fraudulent_card3)
print(num_fraudulent_card3)

# view the percentage of fraudulent_card3 in pie chart ----------
fraudulent_card3_percentage <- (num_fraudulent_card3 / length(unique(data$card3))) * 100
non_fraudulent_card3_percentage <- 100 - fraudulent_card3_percentage
pie_data_card3 <- data.frame(
  Category = c("Fraudulent Card3", "Non-Fraudulent Card3"),
  Percentage = c(fraudulent_card3_percentage, non_fraudulent_card3_percentage)
)
ggplot(pie_data_card3, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Percentage of Fraudulent vs Non-Fraudulent Card3") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5))

# view the count unique value of Card5 where any isFraud == 1 ----------
fraudulent_card5 <- unique(data$card5[data$isFraud == 1])
num_fraudulent_card5 <- length(fraudulent_card5)

# view the percentage of fraudulent_card5 in pie chart ----------
fraudulent_card5_percentage <- (num_fraudulent_card5 / length(unique(data$card5))) * 100
non_fraudulent_card5_percentage <- 100 - fraudulent_card5_percentage
pie_data_card5 <- data.frame(
  Category = c("Fraudulent Card5", "Non-Fraudulent Card5"),
  Percentage = c(fraudulent_card5_percentage, non_fraudulent_card5_percentage)
)
ggplot(pie_data_card5, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Percentage of Fraudulent vs Non-Fraudulent Card5") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5))

# view the distribution of card4 group by isFraud ----------
ggplot(data, aes(x = card4, fill = factor(isFraud))) +
  geom_bar() +
  labs(title = "Distribution of card4 by isFraud",
       x = "card4",
       y = "Count",
       fill = "isFraud") +
  theme_minimal()

# view the percentage of card4 group by isFraud ----------
card4_fraud_percentage <- data %>%
  group_by(card4) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(card4_fraud_percentage)

# visualize the percentage of isFraud and non-fraud by card4 by bar chart ----------
# First, reshape the data for better plotting
card4_plot_data <- card4_fraud_percentage %>%
  select(card4, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

# Create the plot with proper dodged bars ----------
ggplot(card4_plot_data, aes(x = card4, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of Fraudulent and Non-Fraudulent Transactions by card4",
       x = "card4",
       y = "Percentage",
       fill = "Transaction Type")

# view the percentage of card6 group by isFraud ----------
card6_fraud_percentage <- data %>%
  group_by(card6) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(card6_fraud_percentage)


# visualize the count of card6 group by isFraud ----------
ggplot(data, aes(x = card6, fill = factor(isFraud))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of card6 by isFraud",
       x = "card6",
       y = "Count",
       fill = "isFraud") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# visualize the percentage of card6 group by isFraud ----------
card6_plot_data <- card6_fraud_percentage %>%
  select(card6, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  )) 

  # Create the plot with proper dodged bars ----------
ggplot(card6_plot_data, aes(x = card6, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of card6 by isFraud",
       x = "card6",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# Create a comprehensive analysis of card combinations in fraudulent transactions ----------
fraud_data <- data %>% filter(isFraud == 1)

card_combinations <- fraud_data %>%
  mutate(
    has_card2 = !is.na(card2),
    has_card3 = !is.na(card3), 
    has_card5 = !is.na(card5)
  ) %>%
  group_by(has_card2, has_card3, has_card5) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    combination = case_when(
      has_card2 & has_card3 & has_card5 ~ "All Three Cards",
      has_card2 & has_card3 & !has_card5 ~ "Card2 + Card3",
      has_card2 & !has_card3 & has_card5 ~ "Card2 + Card5", 
      !has_card2 & has_card3 & has_card5 ~ "Card3 + Card5",
      has_card2 & !has_card3 & !has_card5 ~ "Card2 Only",
      !has_card2 & has_card3 & !has_card5 ~ "Card3 Only",
      !has_card2 & !has_card3 & has_card5 ~ "Card5 Only",
      TRUE ~ "None of any Card"
    ),
    percentage = (count / sum(count)) * 100
  )

print(card_combinations)

# Visualize card combinations with proper spacing ----------
ggplot(card_combinations, aes(x = reorder(combination, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Fraudulent Transaction Count by Card Combination",
       x = "Card Combination",
       y = "Number of Transactions") +
  theme_minimal() +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
            hjust = -0.1, size = 3) +

  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +

  theme(
    plot.margin = margin(10, 50, 10, 10),  
    axis.text.y = element_text(size = 10)
  )
  

# view the percentage of ProductCD group by isFraud ----------
productcd_fraud_percentage <- data %>%
  group_by(ProductCD) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(productcd_fraud_percentage)   

# visualize the percentage of ProductCD group by isFraud ----------
productcd_plot_data <- productcd_fraud_percentage %>%
  select(ProductCD, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))
# Create the plot with proper dodged bars ----------
ggplot(productcd_plot_data, aes(x = ProductCD, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of ProductCD by isFraud",
       x = "ProductCD",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")   

# view the percentage of isFraud by addr1 which is.na and not is.na ----------
addr1_fraud_summary <- data %>%
  mutate(addr1_is_na = ifelse(is.na(addr1), "NA", "Not NA")) %>%
  group_by(addr1_is_na) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage = (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(addr1_fraud_summary)

# visualize the percentage of isFraud by addr1 which is.na and not is.na ----------
addr1_plot_data <- addr1_fraud_summary %>%
  select(addr1_is_na, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))
# Create the plot with proper dodged bars
ggplot(addr1_plot_data, aes(x = addr1_is_na, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of isFraud by addr1 NA Status",
       x = "addr1 NA Status",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of P_emaildomain group by isFraud ----------
emaildomain_fraud_percentage <- data %>%
  group_by(P_emaildomain) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(emaildomain_fraud_percentage)

# visualize the percentage of P_emaildomain group by isFraud ----------
emaildomain_plot_data <- emaildomain_fraud_percentage %>%
  select(P_emaildomain, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage),   
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud", 
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))
# Create the plot with proper dodged bars
ggplot(emaildomain_plot_data, aes(x = P_emaildomain, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by =  5)) +
  labs(title = "Percentage of P_emaildomain by isFraud",
       x = "P_emaildomain",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")   

# view the percentage of R_emaildomain group by isFraud ----------
remaildomain_fraud_percentage <- data %>%
  group_by(R_emaildomain) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(remaildomain_fraud_percentage)
# visualize the percentage of R_emaildomain group by isFraud ----------
remaildomain_plot_data <- remaildomain_fraud_percentage %>%
  select(R_emaildomain, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))
# Create the plot with proper dodged bars ----------
ggplot(remaildomain_plot_data, aes(x = R_emaildomain, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of R_emaildomain by isFraud",
       x = "R_emaildomain",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the correlation matrix between column from C1 to C14 vs isFraud ----------
numeric_c_columns <- paste0("C", 1:14)

# Check which columns exist and have variance > 0
existing_c_columns <- intersect(numeric_c_columns, names(data))
valid_c_columns <- c()

for(col in existing_c_columns) {
  if(is.numeric(data[[col]]) && var(data[[col]], na.rm = TRUE) > 0) {
    valid_c_columns <- c(valid_c_columns, col)
  }
}

if(length(valid_c_columns) > 0) {
  # Calculate correlations only for valid columns ----------
  correlations_with_fraud <- data %>%
    select(all_of(valid_c_columns), isFraud) %>%
    cor(use = "complete.obs")
  
  fraud_correlations_C <- correlations_with_fraud[valid_c_columns, "isFraud"]
  
  cat("\nCorrelations between valid C columns and isFraud:\n")
  print(round(fraud_correlations_C, 4))
  
  fraud_corr_df <- data.frame(
    Variable = names(fraud_correlations_C),
    Correlation = as.numeric(fraud_correlations_C)
  )
  
  ggplot(fraud_corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df$Correlation > 0, "lightblue", "salmon")) +
    coord_flip() +
    labs(title = "Correlation of C1-C14 Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)
    
} else {
  cat("No valid C columns found with non-zero variance.\n")
}

# view the correlation matrix between column from D1 to D15 vs isFraud ----------
d_column_names <- paste0("D", c(1,8,9,11))

# Filter to only numeric D columns
numeric_d_columns <- data %>% 
  select(all_of(d_column_names)) %>%
  select_if(is.numeric) %>%
  names()

  # Calculate correlation between each D column and isFraud
  correlations_with_fraud <- data %>%
    select(all_of(numeric_d_columns), isFraud) %>%
    cor(use = "complete.obs")

  fraud_correlations_D <- correlations_with_fraud[numeric_d_columns, "isFraud"]

  cat("\nCorrelations between D1, D8, D9, D11 and isFraud:\n")
  print(round(fraud_correlations_D, 4))
  
  fraud_corr_df <- data.frame(
    Variable = names(fraud_correlations_D),
    Correlation = as.numeric(fraud_correlations_D)
  )
 
  ggplot(fraud_corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df$Correlation > 0, "lightblue", "salmon")) +
    coord_flip() +
    labs(title = "Correlation of D1, D8, D9, D11 Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)

# view the correlation matrix between column from V310 to V314 vs isFraud -----------
V_column_names <- paste0("V", 310:314)

numeric_V_columns <- data %>% 
  select(all_of(V_column_names)) %>%
  select_if(is.numeric) %>%
  names()

  correlations_with_fraud <- data %>%
    select(all_of(numeric_V_columns), isFraud) %>%
    cor(use = "complete.obs")
  
  fraud_correlations_V <- correlations_with_fraud[numeric_V_columns, "isFraud"]

  cat("\nCorrelations between V310, V311, V312, V313, V314 and isFraud:\n")
  print(round(fraud_correlations_V, 4))

  # Create a bar plot showing correlations with isFraud ----------
  fraud_corr_df <- data.frame(
    Variable = names(fraud_correlations_V),
    Correlation = as.numeric(fraud_correlations_V)
  )
 
  ggplot(fraud_corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df$Correlation > 0, "lightblue", "salmon")) +
    coord_flip() +
    labs(title = "Correlation of V310, V311, V312, V313, V314 Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)

# view the percentage of id_12 group by isFraud ----------
id_12_fraud_percentage <- data %>%
  group_by(id_12) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_12_fraud_percentage)

id_12_plot_data <- id_12_fraud_percentage %>%
  select(id_12, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_12_plot_data, aes(x = id_12, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_12 by isFraud",
       x = "id_12",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")   

# view the percentage of id_15 group by isFraud ------------------------------------------------
id_15_fraud_percentage <- data %>%
  group_by(id_15) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_15_fraud_percentage)

id_15_plot_data <- id_15_fraud_percentage %>% 
  select(id_15, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))
# Create the plot with proper dodged bars
ggplot(id_15_plot_data, aes(x = id_15, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_15 by isFraud",
       x = "id_15",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_16 group by isFraud ----------
id_16_fraud_percentage <- data %>%
  group_by(id_16) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_16_fraud_percentage)

id_16_plot_data <- id_16_fraud_percentage %>%
  select(id_16, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_16_plot_data, aes(x = id_16, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_16 by isFraud",
       x = "id_16",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_28 group by isFraud ----------
id_28_fraud_percentage <- data %>%
  group_by(id_28) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_28_fraud_percentage)

id_28_plot_data <- id_28_fraud_percentage %>%
  select(id_28, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_28_plot_data, aes(x = id_28, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_28 by isFraud",
       x = "id_28",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_29 group by isFraud ------------------------------------------------
id_29_fraud_percentage <- data %>%
  group_by(id_29) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_29_fraud_percentage)

id_29_plot_data <- id_29_fraud_percentage %>%
  select(id_29, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_29_plot_data, aes(x = id_29, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_29 by isFraud",
       x = "id_29",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_30 group by isFraud ------------------------------------------------
id_30_fraud_percentage <- data %>%
  group_by(id_30) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_30_fraud_percentage)

id_30_plot_data <- id_30_fraud_percentage %>%
  select(id_30, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_30_plot_data, aes(x = id_30, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_30 by isFraud",
       x = "id_30",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# Convert id_32 into string and view the percentage of id_32 group by isFraud ------------------------------------------------
data <- data %>% mutate(id_32 = as.character(id_32))
id_32_fraud_percentage <- data %>%
  group_by(id_32) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =  (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_32_fraud_percentage)

id_32_plot_data <- id_32_fraud_percentage %>%
  select(id_32, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_32_plot_data, aes(x = id_32, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_32 by isFraud",
       x = "id_32",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_34 group by isFraud ------------------------------------------------
id_34_fraud_percentage <- data %>%
  group_by(id_34) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_34_fraud_percentage)

id_34_plot_data <- id_34_fraud_percentage %>%
  select(id_34, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_34_plot_data, aes(x = id_34, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_34 by isFraud",
       x = "id_34",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")   

# view the percentage of id_35 group by isFraud ------------------------------------------------
id_35_fraud_percentage <- data %>%
  group_by(id_35) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_35_fraud_percentage)

id_35_plot_data <- id_35_fraud_percentage %>%   
  select(id_35, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_35_plot_data, aes(x = id_35, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_35 by isFraud",
       x = "id_35",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")   

# view the percentage of id_36 group by isFraud ------------------------------------------------
id_36_fraud_percentage <- data %>%
  group_by(id_36) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_36_fraud_percentage)

id_36_plot_data <- id_36_fraud_percentage %>%
  select(id_36, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_36_plot_data, aes(x = id_36, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_36 by isFraud",
       x = "id_36",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_37 group by isFraud ------------------------------------------------
id_37_fraud_percentage <- data %>%
  group_by(id_37) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_37_fraud_percentage)

id_37_plot_data <- id_37_fraud_percentage %>%
  select(id_37, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(id_37_plot_data, aes(x = id_37, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_37 by isFraud",
       x = "id_37",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the percentage of id_38 group by isFraud ------------------------------------------------
id_38_fraud_percentage <- data %>%
  group_by(id_38) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(id_38_fraud_percentage)

id_38_plot_data <- id_38_fraud_percentage %>%  
  select(id_38, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))    
# Create the plot with proper dodged bars
ggplot(id_38_plot_data, aes(x = id_38, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of id_38 by isFraud",
       x = "id_38",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")

# view the correlation matrix between column from D1 to D15 vs isFraud ------------------------------------------------
d_column_names <- paste0("id_", c("01", "02",  "05", "06",  "09", "10", 
                                  "11", "12", "13", "14",  "17",  "19", "20",
                                   "30",
                                   "32", "35", "36", "37", "38"))

numeric_id_columns <- data %>% 
  select(all_of(d_column_names)) %>%
  select_if(is.numeric) %>%
  names()


  correlations_with_fraud_id <- data %>%
    select(all_of(numeric_id_columns), isFraud) %>%
    cor(use = "complete.obs")

  fraud_correlations_id <- correlations_with_fraud_id[numeric_id_columns, "isFraud"]


  cat("\nCorrelations between id and isFraud:\n")
  print(round(fraud_correlations_id, 4))
  

  fraud_corr_df_id <- data.frame(
    Variable = names(fraud_correlations_id),
    Correlation = as.numeric(fraud_correlations_id)
  )
 
  ggplot(fraud_corr_df_id, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
    geom_bar(stat = "identity", fill = ifelse(fraud_corr_df_id$Correlation > 0, "lightblue", "salmon")) +
    coord_flip() +
    labs(title = "Correlation of id Variables with isFraud",
         x = "Variables",
         y = "Correlation with isFraud") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7)

# view the percentage of DevType group by isFraud
DevType_fraud_percentage <- data %>%
  group_by(DevType) %>%
  summarise(FraudCount = sum(isFraud), NonFraudCount = n() - sum(isFraud)) %>%
  mutate(FraudPercentage = (FraudCount / (FraudCount + NonFraudCount)) * 100,
         NonFraudPercentage =   (NonFraudCount / (FraudCount + NonFraudCount)) * 100)
print(DevType_fraud_percentage)

DevType_plot_data <- DevType_fraud_percentage %>%
  select(DevType, FraudPercentage, NonFraudPercentage) %>%
  pivot_longer(cols = c(FraudPercentage, NonFraudPercentage), 
               names_to = "Type", 
               values_to = "Percentage") %>%
  mutate(Type = case_when(
    Type == "FraudPercentage" ~ "Fraud",
    Type == "NonFraudPercentage" ~ "Non-Fraud"
  ))

ggplot(DevType_plot_data, aes(x = DevType, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage of DevType by isFraud",
       x = "DevType",
       y = "Percentage of Transactions (%)",
       fill = "Transaction Type")


# only keep the column : 'TxnID', 'isFraud', 'TxnAmt', 'ProductCD', 
# 'addr1', 'card1', 'card2', 'card3', 'card4', 'card5', 'card6', 
# 'P_emaildomain', 'R_emaildomain', 'id_15', 'id_28', 'id_29', 'id_32',
# 'id_34', 'id_35', 'id_36', 'id_37', 'id_38', 'TransactionHour',
# 'TransactionDay', 'TransactionMonth', 'TransactionWeekday', 'Log_TxnAmt', 'fraudulent_card1'

data <- data %>%
  select(TxnID, isFraud, TxnAmt, ProductCD, addr1, card1, card2, card3, card4, card5, card6, 
         P_emaildomain, R_emaildomain, id_15, id_28, id_29, id_32, id_34, id_35, id_36, id_37, id_38,
         TransactionHour, TransactionDay, TransactionMonth, TransactionWeekday,
         Log_TxnAmt, fraudulent_card1)



# add a column to indicate whether addr1 is missing
data <- data %>%
  mutate(addr1_missing = ifelse(is.na(addr1), 0, 1))

# add a column to indicate the factor of TxnAmt with interval of 
# 0-100, 100-200, 200-300, 300-400, 400-500, 500-600, 600-700, 700-800, 800-900, 900-1000 and above 1000 
data <- data %>%
  mutate(TxnAmt_Factor = case_when(
    TxnAmt < 100 ~ "0-100",
    TxnAmt >= 100 & TxnAmt < 200 ~ "100-200",
    TxnAmt >= 200 & TxnAmt < 300 ~ "200-300",
    TxnAmt >= 300 & TxnAmt < 400 ~ "300-400",
    TxnAmt >= 400 & TxnAmt < 500 ~ "400-500",
    TxnAmt >= 500 & TxnAmt < 600 ~ "500-600",
    TxnAmt >= 600 & TxnAmt < 700 ~ "600-700",
    TxnAmt >= 700 & TxnAmt < 800 ~ "700-800",
    TxnAmt >= 800 & TxnAmt < 900 ~ "800-900",
    TxnAmt >= 900 & TxnAmt < 1000 ~ "900-1000",
    TxnAmt >= 1000 ~ "Above 1000",
    TRUE ~ NA_character_
  ))

# convert TxnAmt_Factor to factor
data$TxnAmt_Factor <- factor(data$TxnAmt_Factor, levels = c("0-100", "100-200", "200-300", "300-400", "400-500", "500-600", "600-700", "700-800", " 800-900", "900-1000", "Above 1000"))


# convert the categorial data into factor
data <- data %>%
  mutate(ProductCD = as.factor(ProductCD),
         card1 = as.factor(card1),
         card2 = as.factor(card2),
         card3 = as.factor(card3),
         card4 = as.factor(card4),
         card5 = as.factor(card5),
         card6 = as.factor(card6),
         P_emaildomain = as.factor(P_emaildomain),
         R_emaildomain = as.factor(R_emaildomain),
         id_15 = as.factor(id_15),
         id_28 = as.factor(id_28),
         id_29 = as.factor(id_29),
         id_32 = as.factor(id_32),
         id_34 = as.factor(id_34),
         id_35 = as.factor(id_35),
         id_36 = as.factor(id_36),
         id_37 = as.factor(id_37),
         id_38 = as.factor(id_38)
  )

# check the structure of the data
str(data)


