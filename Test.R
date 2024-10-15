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
the_numbers <- read_csv("theNumbers_set.csv")
head(the_numbers)


top_10k <- read_csv("top_10k_set.csv")


column_netflix_shows <- colnames(netflix_shows)
?read_csv()






































