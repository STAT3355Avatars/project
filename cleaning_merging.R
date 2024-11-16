# import necessary libraries:
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
# --------------------------------------------------------------------------------------
#### Observe Dataset:


# Read each of the 3 respective data sets

netflix_shows <- read_csv("netflix_shows_set.csv",
                          col_names = TRUE,
                          col_types = cols(
                            # parse the columns to specific types : makes data cleaning easier
                            "show_id" = col_factor(),
                            "type" = col_factor(),
                            "title" = col_character(),
                            "director" = col_character(),
                            "cast" = col_character(),
                            "country" = col_factor(),
                            "date_added" = col_date(format = "%B %d, %Y"),
                          "release_year" = col_double(),
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
                          movie_name = col_character(),
                          production_year = col_double(),
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
                      original_title = col_character(),
                      popularity = col_double(),
                      release_date = col_date(format = "%Y-%m-%d"),
                      vote_average = col_double(),
                      vote_count = col_double(),
                      genre = col_factor(),
                      overview = col_character(),
                      revenue = col_double(),
                      runtime = col_double(),
                      tagline = col_factor()
                    ),
                    skip_empty_rows = TRUE,
                    na = c("", NA), # reads empty strings as NA
                  )




# ----------------------------------- Selecting netflix_shows columns
colnames(netflix_shows)
# columns to select: type, title, directior, cast, country, listed_in, release_year
netflix_shows <- netflix_shows %>% 
  select(type, title, director, cast, country, listed_in, release_year)

# ----------------------------------- Selecting top-10k columns
colnames(top_10k)
# columns to select: original_title, release_date, vote_average, vote_count, runtime
top_10k <- top_10k %>% 
  select(original_title, release_date, vote_average, 
    vote_count, runtime)

# ----------------------------------- Selecting the_numbers columns
colnames(the_numbers)
# columns to select: movie_name production_budget, domestic_box_office, international_box_office, rating, genre,  creative_type, production_year
the_numbers <- the_numbers %>% 
  select(movie_name, production_budget, 
    domestic_box_office, international_box_office, 
    rating, genre,
  creative_type, production_year)


# ----------------------------Check for duplicates BEFORE Merging:
# Renaming original_title in top_10k and title in netflix_shows to movie_name
top_10k <- rename(top_10k, "movie_name" = "original_title")
netflix_shows <- rename(netflix_shows, "movie_name" = "title")

# Identify duplicates in the_numbers dataset
duplicated_the_numbers <- the_numbers %>%
  group_by(movie_name, production_year) %>%
  filter(n() > 1) %>%
  ungroup()
# View the duplicated rows
print(duplicated_the_numbers)


# Identify duplicates and summarize the dataset: One duplicate found in the top_10k dataset!
duplicated_top10k <- top_10k %>%
  group_by(movie_name, release_date) %>%
  filter(n() > 1) %>%
  summarise(
    vote_average = mean(vote_average, na.rm = TRUE),
    vote_count = sum(vote_count, na.rm = TRUE),
    runtime = unique(runtime)[1], # Assuming runtime should be unique and consistent across duplicates
    .groups = "drop"
  )
duplicated_top10k <- as.data.frame(duplicated_top10k)

# Get rid of the row filled with NA columns
duplicated_top10k <- duplicated_top10k[complete.cases(duplicated_top10k), ]

# Filter rows in top_10k where movie_name matches any movie_name in duplicated_top10k
top_10k[which(top_10k$movie_name %in% duplicated_top10k$movie_name), ] <-
  duplicated_top10k

number_of_duplicates <- top_10k %>%
  group_by(movie_name, release_date) %>%
  filter(n() > 1)  %>% 
  ungroup() %>% 
  count()

distinct_top10k <- top_10k %>% 
  group_by(release_date) %>% 
  distinct(movie_name, .keep_all = TRUE) %>% 
  ungroup() 

# distinct_top10k[!complete.cases(distinct_top10k), ]

# # Assuming 'distinct_top10k' is your dataset
# cleaned_distinct_top10k <- distinct_top10k[apply(distinct_top10k,
#    1, function(row) !all(is.na(row))), ]

# View the distinct dataset
cleaned_distinct_top10k <- distinct_top10k
print(cleaned_distinct_top10k)

# set top_10k to the distinct top_10k
top_10k <- cleaned_distinct_top10k



# Checking duplicates for netflix shows
duplicated_netflix <- netflix_shows %>%
  group_by(movie_name, release_year) %>%
  filter(n() > 1) %>%
  ungroup() # No duplicates found for netflix shows

# View the duplicated rows
print(duplicated_netflix)




# ---------------------------------------Merge the datasets based on movie_name
# Merging the datasets that share the same movie_name 
# all_merged1 <- the_numbers[casefold(the_numbers$movie_name) %in% casefold(top_10k$movie_name) &
#                           casefold(the_numbers$movie_name) %in% casefold(netflix_shows$movie_name), ]
# Extract the release year from the release_date column 
top_10k <- 
  top_10k %>% mutate(production_year = year(release_date))

the_numbers[the_numbers$movie_name == "Death at a Funeral", ]
top_10k[top_10k$movie_name == "Death at a Funeral", ]

# Merging top10k dataset and the_numbers by movie_name and production_year
all_merged_with_top10k <- top_10k %>%
  inner_join(the_numbers, by = c("movie_name", "production_year"))

# Identify duplicates in all_merged: # No duplicated Names because the production year are different
duplicated_merged <- all_merged_with_top10k %>%
  group_by(movie_name) %>%
  filter(n() > 1)
all_merged_with_top10k_df <- as.data.frame(all_merged_with_top10k) 
all_merged_with_top10k_df %>%
  group_by(movie_name, production_year) %>% 
  filter(n() > 1) %>% 
  select(movie_name, release_date, vote_average, vote_count, production_year)

as.data.frame(all_merged_with_top10k) %>% 
  head()

netflix_shows <- netflix_shows %>% 
  mutate(production_year = release_year)

all_merged_with_top10k <- all_merged_with_top10k %>%
  inner_join(netflix_shows, by = c("movie_name", "production_year"))

all_merged_with_top10k_df <- as.data.frame(all_merged_with_top10k)

# There are no re-curring movies so, no repeated rows
all_merged_with_top10k_df %>%
  group_by(movie_name, production_year) %>%
  filter(n() > 1) %>% 
  ungroup()

# Now, there is no point in summarising duplicates because there are no duplicates
summarised_duplicates <- all_merged_with_top10k_df %>%
  group_by(movie_name, production_year) %>%
  filter(n() > 1) %>%
  summarise(
    release_date = first(release_date),
    vote_average = round(mean(vote_average, na.rm = TRUE), 1),
    vote_count = sum(vote_count, na.rm = TRUE),
    runtime = max(runtime, na.rm = TRUE),
    .groups = "drop"
  )

# Get unique rows (including those without duplicates)
unique_rows <- all_merged_with_top10k_df %>%
  group_by(movie_name, production_year) %>%
  filter(n() == 1) %>%
  ungroup()

# There was no need to combine summarised_duplicates and unique rows. Check that number of unique rows are equal to number or all_merged_with_top10k_df
nrow(all_merged_with_top10k_df) == nrow(unique_rows)  # must be TRUE

# Get the final dataframe
final_df <- all_merged_with_top10k_df


# Assuming 'final_df' is your dataframe
na_rows <- as_tibble(final_df[!complete.cases(final_df), ])
# View the rows with NA values
print(na_rows) # Surprisingly, there are no rows with NA values after merging. YAY!!!


colnames(final_df)
# There should be 18 columns in the final dataframe:

# Export final_df as .csv file
write_csv(
  x = all_merged,
  file = "merged_data.csv")



# The cleaned and extracted codes are above the dashed line
# --------------------------------------------------------------------------------------------------
# HI! I want to delete the rest from here to the buttom. 
