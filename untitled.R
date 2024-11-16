library(readr)
library(dplyr)
library(ggplot2)

data <- read_csv("merged_data.csv")


data$profit_margin <- data$domestic_box_office + 
  data$international_box_office - data$production_budget

ninetieth <- quantile(data$profit_margin, 0.9)

successful_films <- data[data$profit_margin >= ninetieth, ]

unsuccessful_films <- data[data$profit_margin < ninetieth, ]


# Now that I have the successful and unsuccessful subsets of the data
# We can start doing interesting stuff (unprofessional comments, I know
# but only you guys are reading this..)

ggplot(data = successful_films, aes(x = genre)) + 
  geom_bar() +
  labs(title = "The number of exceptionally successful films in each genre!")

ggplot(data = unsuccessful_films, aes(x = genre)) +
  geom_bar() + 
  labs(title = "The number of other films in each genre!")


