
library(readr)
library(dplyr)
library(ggplot2)

# To merge datasets:

# ---------------------------------------------------------------------------

# Next Steps:

# Dataset has been transforemd using log-transformation. Log-transformation
# compress data, especially extreme values. It scales them down to make the data more manageable and less skewed.

# 1: Perform Descriptive Statistics: Get a description and insight from our data
        # Suggestion: Use original (merged and untransformed - no log transformation) dataset and log-transformed dataset for descriptive statistics: we can compare the insights on both and compare.
#  Descriptive stats on the original data give a baseline, while stats on the log-transformed data offer insights on how transformations affect the data.


# 2: Visualize Descriptive Statistics : Visualize the described data:
        # Suggestion: To check trends over a period of time, look into Linecharts.
        # For categorical data: Look at Piecharts, Bargraphs and Cummulative Frequency Charts

# 3: Get a Good Sample and Normalize the data: Get the normal distribution 

# 4: Perform Inferential Statistics: Deal with Uncertainty and Test the Sample.
        # Suggestion: Look into Hypothesis Testing, One-Sample t-test, Two Sample  
        #    tests,  Tests for Proportions: Which is the best for our purpose(s)?

# 5: Find the Correlation and Regression between Multiple Variables:
        #  Suggestion: Look into Scatterplots, Heatmap, Corrplots 
       

# 6: Look into Lag Analysis/Time Series Analysis:



# --------------------------------------------------------------------------

# 1. Perform Descriptive Statistics: 
# (a) The Center of the data : Mean, Median, Mode (We don't need Mode per se)?
  # example: What is the [insert tool e.g "mean"] of the [insert numerical      
              # variable e.g "revenue"]:
  # General Question: for all movies in all years? for all movies in each year?
  # Categorized question: for all movies in each genre? for each genre in each year?

# Mean, Median, SD, Variance

data <- read_csv("merged_data.csv")

data %>% 
        summary() # We have missing values we have to deal with first.

# (b) Find the Measures of Dispersion:
        # Suggestion: Get the Range, Interquartile Range, Standard Deviation,
        # Variance, Coefficient of Variation? What do they tell us?


# boxplot 1:"Box Plot of Revenue for each Genre"

colnames(data)


data %>% 
        ggplot(data = .,
                mapping = aes(y = revenue, color = genre)) +
        geom_boxplot() +
        ggtitle("Box Plot of Revenue for each Genre")

# boxplot: "Box Plot of Runtime vs Revenue for each Genre"
data %>%
  ggplot(
    data = .,
    mapping = aes(x = revenue, 
        y = runtime, color = genre)
  ) +
  geom_boxplot() +
  ggtitle("Box Plot of Runtime vs Revenue for each Genre")


# barplot version 1: "Bar Plot of Genre for each Rating"

data %>%
  ggplot(
    data = .,
    mapping = aes(y = genre, fill = rating)
  ) +
  geom_bar(position = "stack") +
  ggtitle("Bar Plot of Genre for each Rating")

# barplot 2: "Bar Plot of each Creative Type and their Genre"
data$creative_type <- factor(data$creative_type)
data %>%
  ggplot(
    data = .,
    mapping = aes(y = creative_type, fill = rating)
  ) +
  geom_bar(position = "stack") +
  ggtitle("Bar Plot of each Creative Type and their Genre")

# barplot 2: "Bar Plot of each Creative Type and their Genre"
data$creative_type <- factor(data$creative_type)
data %>%
  ggplot(
    data = .,
    mapping = aes(y = rating, fill = genre),
    colour = "black"
  ) +
  geom_bar(position = "stack") +
  ggtitle("Bar Plot of each Creative Type and their Genre")


# Histogram:
data %>% 
        ggplot(data = .,
                mapping = aes(x = revenue,
                fill = genre)) +
        geom_histogram(color = "black", na.rm = FALSE)

# Density Curve: Density Curve of Revenue
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = revenue)
  ) +
  geom_density(alpha = 0.8) +
        ggtitle("Density Curve of Revenue")

# Density Curve: Density Curve of Revenue based on Rating
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = revenue,
      fill = rating
    )
  ) +
  geom_density(alpha = 0.8) +
  ggtitle("Density Curve of Revenue")


# Linechart 1: Trend of Revenues over the Years
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = release_date,
      y = revenue
    )
  ) +
  geom_line() +
  ggtitle("Trend of Revenues over the Years")

# Line Chart 2: Trend of Revenues over the Years for Each Genre
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = release_date,
      y = revenue,
      color = genre
    )
  ) +
  geom_line() +
  ggtitle("Trend of Revenues over the Years for Each Genre")

# Line Chart 3: Trend of Revenues over the Years for Each Rating
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = release_date,
      y = revenue,
      color = rating
    )
  ) +
  geom_line() +
  ggtitle("Trend of Revenues over the Years for Each Rating")

# Barplot: Barplot of Budget for Each Rating
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = production_budget,
 fill = rating
    )) +
  geom_histogram(color = "black") +
  ggtitle("Barplot of Budget for Each Rating")


# Barplot: Barplot of Budget for Each Genre
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = production_budget,
      fill = genre
    )
  ) +
  geom_histogram(color = "black") +
  ggtitle(" Barplot of Budget for Each Genre")


# Tuesday: Split the years into intervals (every five years maybe),
# and look at the trend for each genre in every interval.


# (c) Find the Measures of Spread/Dispersion: How?
    # Suggestion: Look into Box-Plots, Violin Plots, Histograms,
                # Cummulative Frequency curves (for categorical data) / Maybe barcharts, grouped bar charts and stacked barcharts?


# (d) What is the Shape of the data? Define the Skewness of the data. How?
    # Suggestion: Every distribution can be standardized. Standardize the (log-transformed) datasets then visualize the standardized dataset to see the distribution.































































