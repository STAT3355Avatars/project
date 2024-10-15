# import necessary libraries:
library(readr)
library(dplyr)

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

# 2. theNumbers dataset
the_numbers <- read_csv("theNumbers_set.csv",
                        col_types = cols(
                          movie_name = col_factor(),
                          production_year = col_factor(),
                          movie_odid = col_factor(),
                          production_budget = col_factor(),
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

top_10k <- read_csv("top_10k_set.csv")


column_netflix_shows <- colnames(netflix_shows)
?read_csv()






































