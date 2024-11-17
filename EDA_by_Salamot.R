library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)


# Import the merged_data.csv file
data <- read.csv("merged_data.csv")

# There should be 237 rows and 18 columns: Check:
nrow(data) # 17 rows confirmed
ncol(data) # 18 columns confirmed

colnames(data) # Get the column names

# Calculating Proft and Profit-to-Cost Ratio:
data <- data %>% 
  mutate(revenue = international_box_office + domestic_box_office,
    profit = (international_box_office + domestic_box_office) - production_budget,
        profit_to_cost_ratio = (profit / production_budget)) 
# %>% 
  # select(movie_name, genre, profit, profit_to_cost_ratio, production_budget) 


# ---------------------------- Barplot of Profit vs Genre
data %>% 
  group_by(genre) %>% 
  summarise(profit_sum = sum(profit)) %>% 
  ggplot(data = ., mapping = aes(x = genre, 
    y = profit_sum,
    fill = genre)) +
  geom_bar(stat = "identity", color = "white")

# Checking Profitability efficiency accross each genre, on average:
data %>% 
  group_by(genre) %>% 
  summarise(mean_profit_to_cost_ratio = mean(profit_to_cost_ratio, 
                                    na.rm = TRUE)) %>%
  ggplot(data = ., mapping = aes(x = genre, 
                          y = mean_profit_to_cost_ratio,
                          fill = genre)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Average Profit-to-Cost Ratio by Genre", y = "Profit-to-Cost Ratio") +
  theme_minimal()

# ----------------------------  Side-by-Side Barplots:
# Summarize the data by genre
data_summarized <- data %>%
  group_by(genre) %>%
  summarise(
    total_production_budget = sum(production_budget, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE)
  )

# Reshape the data from wide format to long format
data_long <- data_summarized %>%
  pivot_longer(
    cols = c(total_production_budget, total_profit),
    names_to = "variable",
    values_to = "value"
  )

# Create the side-by-side bar plot
ggplot(data = data_long, mapping = aes(x = genre, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Production Budget and Profit by Genre",
    y = "Amount ($)", x = "Genre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ---------------------------------Scatterplots
# Scatter Plot of Production_Budget and Profit
data %>%
  ggplot(
    data = .,
    mapping = aes(
      x = profit,
      y = production_budget, color = genre
    )
  ) +
  geom_point()

# Scatter Plot of Production_Budget and Profit-to-Cost Ratio
data %>%
  ggplot(data = .,
  mapping = aes(x = profit_to_cost_ratio,
                  y = production_budget, color = genre)) +
  geom_point()


# -------------------------- Visualize the Density Distribution
# Get the profit distribution across Genre: Histogram of Profit based on Genre
data %>% 
  ggplot(data = ., 
  mapping = aes(x = profit)) +
  geom_density(alpha = 0.8) +
  ggtitle("Density Curve of Profit") # Right Skewed Distribution

# Get the profit-to-cost-ratio distribution across Genre: Histogram of Profit-to-cost-ratio based on Genre
data %>%
  ggplot(
    data = .,
    mapping = aes(x = profit_to_cost_ratio)
  ) +
  geom_density(alpha = 0.8) +
  ggtitle("Density Curve of Profit") # Right Skewed Distribution



# -------------------------- Splitting Data to Successful and Unsuccessful films
# Get the 75th percentile for the profit distribution:
profit_75th_percentile <- quantile(data$profit, 0.75)
successful_films <- data[data$profit >= profit_75th_percentile, ]
unsuccessful_films <- data[data$profit < profit_75th_percentile, ]
# Bar plot of Genre vs Count
ggplot(data = successful_films, aes(x = genre,
fill = genre)) +
  geom_bar() +
  labs(title = "The number of exceptionally successful films in each genre!") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(
  stat = "count", aes(label = after_stat(count)),
  vjust = -0.5, size = 5
)


ggplot(data = unsuccessful_films, aes(x = genre,
fill = genre)) +
  geom_bar() +
  labs(title = "The number of other films in each genre!") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(
  stat = "count", aes(label = after_stat(count)),
  vjust = -0.5, size = 5
)



# Get the 75th percentile for the profit distribution:
ratio_75th_percentile <- quantile(data$profit_to_cost_ratio, 0.75)
ratio_successful_films <- data[data$profit_to_cost_ratio >= ratio_75th_percentile, ]
ratio_unsuccessful_films <- data[data$profit_to_cost_ratio < ratio_75th_percentile, ]
# Bar plot of Genre vs Count for Successful Films based on Profit-cost-ratio
ggplot(data = ratio_successful_films, 
  aes(x = genre, fill = genre)) +
  geom_bar() +
  labs(title = "The number of successful films with high profit-cost-ratio in each genre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +geom_text(
  stat = "count", aes(label = after_stat(count)),
  vjust = -0.5, size = 5)

# Create the Genre vs Count for Unsuccessful Films based on Profit-cost-ratio
ggplot(data = ratio_unsuccessful_films, aes(x = genre, fill = genre)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), 
  vjust = -0.5, size = 5) +
  labs(title = "The number of other films with high profit-cost-ratio in each genre"
) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


# --------------------------------------EDA on Exceptionally Successful Films!
# For each year:
# Summarize the data by release year and calculate total profit
data_summary <- data %>%
  group_by(release_year, genre) %>%
  summarise(
    total_profit = sum(profit, na.rm = TRUE),
    cost = sum(production_budget, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE),
    most_occuring_creative_type = mode(creative_type),
    most_occuring_rating = mode(rating),
    mean_vote_average = mean(vote_average, na.rm = TRUE),
    total_vote_count = sum(vote_count, na.rm = TRUE),
    mean_runtime = mean(runtime, na.rm = TRUE),
  ) %>%
  mutate(
    year_over_year_profit = total_profit - lag(total_profit),
    year_over_year_revenue = total_revenue - lag(total_revenue),
    year_over_year_cost = cost - lag(cost)  
  ) %>%
  filter(!is.na(year_over_year_profit),
        !is.na(year_over_year_revenue),
        !is.na(year_over_year_cost)) # Remove rows with NA in year_over_year variables

data_summary %>%
  ungroup() %>%
  ggplot(aes(x = release_year, y = year_over_year_profit, fill = as.factor(release_year))) +
  geom_bar(stat = "identity") +
  labs(title = "Year-over-Year Profit Change", y = "Year-over-Year Profit", x = "Release Year") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(
    "#052f36",
    "#06343d", "#053e4a", "#0c505a", "#075561", "#0d6c7b", "#0f7a8b", "#2192a3", "#0c9cb3", "#10a5bc", "#099eb5", "#05a7c0", "#05b1cb",
    "#04b8d3"
  )) +
  geom_line(aes(group = 1), color = "red", size = 1)# 


year_over_year_long <- data_summary %>%
  select(release_year, genre, year_over_year_profit, year_over_year_revenue, year_over_year_cost) %>%
  pivot_longer(cols = c(year_over_year_profit), names_to = "profit", values_to = "value")


ggplot(data = year_over_year_long, aes(x = release_year, y = value, fill = profit)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_line(aes(group = 1), color = "blue", size = 1) + 
  labs( title = "Year-over-Year Profit Changes by Genre", y = "Value", x = "Release Year" ) + theme_minimal() + theme(legend.title = element_blank())





# Create the bar plot 
ggplot(data = year_over_year_long, aes(x = release_year, y = value, fill = release_year)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~genre, scales = "free_y") + 
  labs( title = "Year-over-Year Profit Changes by Genre", y = "Value", x = "Release Year" ) + 
  theme_minimal() + theme(legend.title = element_blank())

# Create the bar plot with a trend line
ggplot(data = year_over_year_long, aes(x = release_year, y = value, fill = release_year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~genre, scales = "free_y") +
  labs(
    title = "Year-over-Year Profit Changes for Each Genre",
    y = "Value",
    x = "Release Year"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())




# Create the line chart
ggplot(data = year_over_year_long, aes(x = release_year, y = value, color = genre)) +
  geom_line(size = 1) + # Add trend line
  geom_point(size = 2) + # Optional: Add points to the line
  labs(
    title = "Year-over-Year Profit Changes by Genre",
    y = "Year-over-Year Profit",
    x = "Release Year"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())







# Assuming your data has a release_date column
data <- data %>%
  mutate(release_month = month(release_date, label = TRUE, abbr = TRUE))


# Summarize data by release month
monthly_profit_summary <- data %>%
  group_by(release_month) %>%
  summarise(
    total_profit = sum(profit, na.rm = TRUE)
  ) %>%
  arrange(release_month)

# Create the trend line chart for monthly profit
ggplot(data = monthly_profit_summary, aes(x = release_month, y = total_profit, group = 1)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "blue") +
  labs(
    title = "Total Profit by Month of the Year",
    y = "Total Profit",
    x = "Month"
  ) +
  theme_minimal()





# Assuming your data has a release_date column
data <- data %>%
  mutate(
    release_year = year(release_date),
    release_month_num = month(release_date),
    year_month = as.Date(paste(release_year, release_month_num, "01", sep = "-"))
  )

# Summarize data by year and month
monthly_profit_summary <- data %>%
  group_by(year_month) %>%
  summarise(
    total_profit = sum(profit, na.rm = TRUE)
  ) %>%
  arrange(year_month)

# Create the trend line chart for monthly profit
ggplot(data = monthly_profit_summary, aes(x = year_month, y = total_profit, group = 1)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "blue") +
  labs(
    title = "Total Profit by Month and Year",
    y = "Total Profit",
    x = "Date"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 year") # Format the x-axis labels





# Assuming your data has a release_date column
data <- data %>%
  mutate(
    release_year = year(release_date),
    release_month = month(release_date, label = TRUE, abbr = TRUE),
    release_month_num = month(release_date), # Create a numeric month for date creation
    year_month = as.Date(paste(release_year, release_month_num, "01", sep = "-"))
  )
# Summarize data by year and month for the trendline
monthly_profit_trend <- data %>%
  group_by(year_month, genre) %>%
  summarise(
    total_profit = sum(profit, na.rm = TRUE)
  ) %>%
  arrange(year_month)

# Create the box plot with trendline
ggplot(data = data, aes(x = year_month, y = profit)) +
  geom_boxplot(aes(group = year_month), fill = "lightblue") +
  geom_point(data = monthly_profit_trend, aes(x = year_month, y = total_profit, group = 1, color = genre)) +
  labs(
    title = "Total Profit by Month and Year",
    y = "Total Profit",
    x = "Date"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 year") # Format the x-axis labels







