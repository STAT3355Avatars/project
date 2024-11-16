library(tidyr)
library(ggplot2)
library(dplyr)

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



# Rank by Success:


























