# import necessary libraries:
library(readr)
library(dplyr)
library(ggplot2)

# --------------------------------------------------------------------------------------
#### Observe Dataset:


# Read each of the 3 respective data sets

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
top_10k <- read_csv("top_10k_set.csv",
                    col_select = c(-"...1"), # exclude first column
                    col_types = cols(
                      id = col_double(),
                      original_language = col_factor(),
                      original_title = col_factor(),
                      popularity = col_double(),
                      release_date = col_date(format = "%Y-%m-%d"),
                      vote_average = col_double(),
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

# The three dataset are in tibble. Feel free to convert them to dataframe as you work on it:
netflix_shows %>% 
  as.data.frame()

the_numbers %>% 
  as.data.frame()

top_10k %>% 
  as.data.frame()


top_10k <- top_10k %>% select(-genre)
top_10k <- rename(top_10k, "movie_name" = "original_title")
netflix_shows <- rename(netflix_shows, "movie_name" = "title")

all_merged <- the_numbers[casefold(the_numbers$movie_name) %in% casefold(top_10k$movie_name) &
                          casefold(the_numbers$movie_name) %in% casefold(netflix_shows$movie_name), ]



filtered_the_numbers <- the_numbers %>% 
  filter(movie_name %in% top_10k$movie_name &
        movie_name %in% netflix_shows$movie_name)

filtered_top_10k <- top_10k %>% 
  filter(movie_name %in% the_numbers$movie_name & 
        movie_name %in% netflix_shows$movie_name)

filtered_netflix_shows <- netflix_shows %>%
  filter(movie_name %in% top_10k$movie_name &
        movie_name %in% the_numbers$movie_name)

top10k_numbers_merged <- inner_join(filtered_top_10k, filtered_the_numbers, by="movie_name")

all_merged <- inner_join(filtered_netflix_shows, top10k_numbers_merged)

# Create a graph:

# color vector for revenue (hex codes used for 
# ease of visualizing data)

colors_revenue <- c("4"="red","5"="#FF4000","6"="#FF8000",
"7"="#FFBF00","8"="#80FF00","9"="green")


top10k_numbers_merged %>%
  ggplot(data = .,
  mapping = aes(x = release_date))


write_csv(x = all_merged,
          file = "merged_data.csv")

























