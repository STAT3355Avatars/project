# import necessary libraries:
library(readr)
library(dplyr)
library(ggplot2)

# --------------------------------------------------------------------------------------
#### Observe Dataset:

# 1. netflix_shows.test.csv
netflix_shows <- read_csv("netflix_shows_set.csv",
                          col_names = TRUE,
                          col_types = cols(
                            # parse the columns to specific types : makes data cleaning easier
                            "show_id" = col_factor(),
                            "type" = col_factor(),
                            "title" = col_factor(),
                            "director" = col_character(),
                            "cast" = col_character(),
                            "country" = col_factor(),
                            "date_added" = col_date(format = "%B %d, %Y"),
                          "release_year" = col_factor(),
                          "rating" = col_factor(),
                          "duration" = col_factor(),
                          "listed_in" = col_factor(),
                          "description" = col_factor(),
                          ),
                        skip_empty_rows = TRUE, # skips empty rows
                      na = c("", NA), # reads empty strings as NA
                    )
# check first 6 rows:
head(netflix_shows)
spec(netflix_shows) # returns the columns and their types


# 2. theNumbers dataset
the_numbers <- read_csv("theNumbers_set.csv",
                        col_types = cols(
                          movie_name = col_factor(),
                          production_year = col_factor(),
                          movie_odid = col_factor(),
                          production_budget = col_double(),
                          domestic_box_office = col_double(),
                          international_box_office = col_double(),
                          rating = col_factor(),
                          creative_type = col_factor(),
                          source = col_factor(),
                          production_method = col_factor(),
                          genre = col_factor(),
                          sequel = col_factor(),
                          running_time = col_factor()
),
                        skip_empty_rows = TRUE,
                        na = c("", NA), # reads empty strings as NA
                        )

# check the numbers dataset:
head(the_numbers)
spec(the_numbers) # returns the columns and their types


# 3. top_10k dataset
top_10k <- read_csv("top_10k_set.csv",
                    col_select = c(-"...1"), # exclude first column
                    col_types = cols(
                      id = col_double(),
                      original_language = col_factor(),
                      original_title = col_factor(),
                      popularity = col_double(),
                      release_date = col_date(format = "%Y-%m-%d"),
                      vote_average = col_factor(),
                      vote_count = col_factor(),
                      genre = col_factor(),
                      overview = col_character(),
                      revenue = col_double(),
                      runtime = col_factor(),
                      tagline = col_factor()
                    ),
                    skip_empty_rows = TRUE,
                    na = c("", NA), # reads empty strings as NA
                  )

# check the top_10k dataset:
head(top_10k)
spec(top_10k) # returns the columns and their types


# The three dataset are in tibble. Feel free to convert them to dataframe as you work on it:
netflix_shows %>% 
  as.data.frame()

the_numbers %>% 
  as.data.frame()

top_10k %>% 
  as.data.frame()


top10k_numbers_merged <- the_numbers[tolower(the_numbers$movie_name) %in% tolower(top_10k$original_title), ]
top_10k <- rename(top_10k, "movie_name" = "original_title")

head(top_10k)

filtered_the_numbers <- the_numbers %>% 
  filter(movie_name %in% top_10k$movie_name)

filtered_top_10k <- top_10k %>% 
  filter(movie_name %in% the_numbers$movie_name)

top10k_numbers_merged <- inner_join(filtered_top_10k, filtered_the_numbers, by="movie_name")
# factor(top10k_numbers_merged$movie_name) One or more movies is counted more than once
# length(top10k_numbers_merged$movie_name)
colnames(top10k_numbers_merged)

top10k_numbers_merged <- top10k_numbers_merged %>% 
  mutate(revenue = round(log10(domestic_box_office + international_box_office)),
        popularity_discrete = round(log(popularity, 2))) %>% 
  select(movie_name, production_budget, revenue, vote_average, popularity)


# Create a graph:
top10k_numbers_merged %>% 
  ggplot(data = .,
  mapping = aes(x = vote_average,
  y = production_budget,
color = revenue, 
alpha = popularity)) +
  geom_point(size = 3)

top10k_numbers_merged


# May come back to this for data cleaning. For now just try to make the 8 graphs first
normalize <- function(a, b){
  
}

























