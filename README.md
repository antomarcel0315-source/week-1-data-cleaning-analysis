# week-1-data-cleaning-analysis
# ============================================================
# Week 1: Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic Passenger Dataset
# ============================================================

# ---------------------------
# 1. Load Required Libraries
# ---------------------------

library(dplyr)
library(ggplot2)


# ---------------------------
# 2. Load Dataset
# ---------------------------

titanic <- read.csv("titanic.csv")


# ---------------------------
# 3. Initial Data Assessment
# ---------------------------

# Number of rows and columns
dim(titanic)

# Structure of the dataset
str(titanic)

# Summary statistics
summary(titanic)

# Check missing values
colSums(is.na(titanic))


# ---------------------------
# 4. Remove Duplicate Records
# ---------------------------

titanic <- distinct(titanic)


# ---------------------------
# 5. Handle Missing Values
# ---------------------------

# Replace missing Age values with median
titanic$Age[is.na(titanic$Age)] <-
  median(titanic$Age, na.rm = TRUE)

# Replace missing Fare values with median
titanic$Fare[is.na(titanic$Fare)] <-
  median(titanic$Fare, na.rm = TRUE)

# Replace missing Embarked values with the most frequent category
titanic$Embarked[is.na(titanic$Embarked)] <-
  names(which.max(table(titanic$Embarked)))

# Replace missing Cabin values with "Unknown"
titanic$Cabin[is.na(titanic$Cabin)] <- "Unknown"


# ---------------------------
# 6. Detect and Cap Outliers
#    Using 1.5 × IQR Rule
# ---------------------------

cap_iqr <- function(x) {

  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)

  iqr_value <- q3 - q1

  lower_limit <- q1 - 1.5 * iqr_value
  upper_limit <- q3 + 1.5 * iqr_value

  pmin(
    pmax(x, lower_limit),
    upper_limit
  )
}

# Apply IQR capping
titanic$Age <- cap_iqr(titanic$Age)
titanic$Fare <- cap_iqr(titanic$Fare)


# ---------------------------
# 7. Min-Max Normalization
# ---------------------------

minmax <- function(x) {

  (x - min(x)) /
    (max(x) - min(x))
}

# Create normalized variables
titanic$Age_norm <- minmax(titanic$Age)
titanic$Fare_norm <- minmax(titanic$Fare)


# ---------------------------
# 8. Categorical Encoding
# ---------------------------

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)


# ---------------------------
# 9. Exploratory Data Analysis
# ---------------------------

# Summary after cleaning
summary(titanic)

# Correlation between numerical variables
cor(
  titanic[, c("Age", "SibSp", "Parch", "Fare")]
)


# ---------------------------
# 10. Visualizations
# ---------------------------

# Age distribution
ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 25) +
  labs(
    title = "Distribution of Passenger Age",
    x = "Age",
    y = "Number of Passengers"
  ) +
  theme_minimal()


# Survival by Passenger Class
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  labs(
    title = "Survival by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers",
    fill = "Survived"
  ) +
  theme_minimal()


# Survival by Gender
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  labs(
    title = "Survival by Gender",
    x = "Gender",
    y = "Number of Passengers",
    fill = "Survived"
  ) +
  theme_minimal()


# ---------------------------
# 11. Final Data Check
# ---------------------------

# Check remaining missing values
colSums(is.na(titanic))

# Check final structure
str(titanic)

# View cleaned dataset
head(titanic)