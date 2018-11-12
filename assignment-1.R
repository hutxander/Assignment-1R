library(tidyverse)

# data_analysis_1 ---------------------------------------------------------
# Data is read from the same folder as this script.
# In the read_csv, we change missing age values ("-") to NA and make age numeric.
forbes_data <- read_csv(
  "forbes.csv",
  na = c("-"),
  col_types = cols(
    age = col_integer()
  )
) %>%

  # We change rank and net_worth to numerics outside of read_csv.
  # This is because of additional work to be done (taking care of #, $, B/M, digits.)
  # rank first. Delete # with separate() and use mutate to create integer.
  separate(
    col = "rank",
    into = c(NA, "rank")
  ) %>%
  mutate(
    rank = as.integer(rank)
  ) %>%

  # Now net_worth. Use seperate twice, first to split numbers from B/M sign.
  # Second time to delete dollar sign (first character, so sep = 1).
  # Finally, mutate to create numeric and delete created BM column.
  separate(
    col = "net_worth",
    into = c("net_worth", "BM"),
    sep = c(" ")
  ) %>%
  separate(
    col = "net_worth",
    into = c(NA, "net_worth"),
    sep = 1
  ) %>%
  mutate(
    net_worth = as.double(net_worth),
    net_worth = net_worth * if_else(BM %in% c("B"), 1000000000, 1000000),
    BM = NULL
  )


# data_analysis_2 ---------------------------------------------------------
# Note: this only works if forbes_data is in Environment
# Filter deletes rows with less than $1B
# This filtered dataset is saved separately, since we want to return to forbes_data later on.
forbes_data_filtered <- filter(
  forbes_data,
  net_worth >= 1000000000
)


# data_analysis_3 ---------------------------------------------------------
# Note: this only works if forbes_data is in Environment
# Use GGplot twice, age on net_worth and log(net_worth).
# Positive relationship between age and net_worth, clearer from log regression.
# Extra clarity (log) due to the decreased effect of extremely high net_worth observations.
ggplot(data = forbes_data_filtered, mapping = aes(x = age, y = net_worth)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = forbes_data_filtered, mapping = aes(x = age, y = log(net_worth))) +
  geom_point() +
  geom_smooth(se = FALSE)


# data_analysis_4 ---------------------------------------------------------
country_statistics <- forbes_data_filtered %>%
  # Group by country
  group_by(country) %>%
  # Make new data - observations per country, max, min and diff net_worth
  summarize(
    count = n(),
    max(net_worth),
    min(net_worth),
    max(net_worth) - min(net_worth)
  ) %>%
  # Filter out countries with less than 6 observations
  filter(
    count >= 6
  )

# Per country, find difference between highest and lowest person on the list.


# Sort by ascending order.



# data_analysis_5 ---------------------------------------------------------

ggplot(data = forbes_data_filtered, mapping = aes(x = diff_country, y = count())) +
  geom_bar()
# Adjust labels of the countries!

# data_analysis_6 ---------------------------------------------------------



# data_analysis_7 ---------------------------------------------------------
# I assume we have to go back to the original dataset - so I will work on with forbes_data from the end of data_analysis_1

# If unique(forbes_data::rank = rows(forbes_data)) then no double ranks, else there are.
# Which are double? make column 1:1578, then take that column minus the rank column.
# Everywhere that we end up with something else than zero, the rank is double.
# Specifically, the row before the one with value 1 is the rank that is held more than once.

# data_analysis_8 ---------------------------------------------------------
# The average is equal to the original rank +0.5 times the amount of people that keep this rank (not +0.5 for single person holding the rank).


# data_analysis_9 ---------------------------------------------------------
# Step 1: compute the sum of net_worth per country.
# Step 2: Plot this sum on the world map.
