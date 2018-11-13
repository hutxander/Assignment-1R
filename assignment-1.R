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
  # Rename new columns
  rename(
    max_worth = "max(net_worth)",
    min_worth = "min(net_worth)",
    diff_worth = "max(net_worth) - min(net_worth)"
  ) %>%
  # Filter out countries with less than 6 observations
  filter(
    count >= 6
  ) %>%
  # Sort by ascending difference
  arrange(diff_worth)


# data_analysis_5 ---------------------------------------------------------
# ggplot with country_statistics from 4. Stat has to be changes from bin to use numeric y.
# theme is used to adjust x-labels to vertical text.
ggplot(data = country_statistics, mapping = aes(x = country, y = diff_worth)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# data_analysis_6 ---------------------------------------------------------
# Essentially the same as 5, but added reorder for ascending difference.
# Also, axis labels are adjusted.
ggplot(data = country_statistics, mapping = aes(x = reorder(country, diff_worth), y = diff_worth)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Country", y = "Difference Net Worth ($)")


# data_analysis_7 ---------------------------------------------------------
# I assume we have to go back to the original dataset - so I will work on with forbes_data from the end of data_analysis_1
shared_ranks <- forbes_data %>%
  # Group by rank
  group_by(rank) %>%
  # Make new data - observations per rank - if higher than 1 --> double rank
  summarize(
    count = n()
  ) %>%
  # Filter out ranks that occur only once
  filter(
    count >= 2
  )
# shared_ranks now includes the shared ranks and number of occurences.

# data_analysis_8 ---------------------------------------------------------
# Use the shared_ranks data made in 7 and the original forbes_data.
# Join shared ranks based on rank to get a column with shared rank vales at the right place.
rank_types <- left_join(x = forbes_data, y = shared_ranks, by = "rank") %>%
  rename(
    average_rank = "count",
    original_rank = "rank"
  ) %>%
  # Delete all data we do not need by taking only the ranks in the selection
  select(
    original_rank,
    average_rank
  ) %>%
  mutate(
    # The average is equal to the original rank +0.5 for everyone the rank is shared with.
    # First compute the addition for shared ranks, then change NA to 0.
    # From this, the column average_rank is equal to the addition that should be given to original_rank.
    # Adding original_rank results in the average rank. 
    average_rank = (average_rank - 1) / 2,
    average_rank = ifelse(is.na(average_rank), 0, average_rank),
    average_rank = average_rank + original_rank
  )
# rank_types now contains the two rank types next to eachother.


# data_analysis_9 ---------------------------------------------------------
# Again, I assume I should use the initial dataset. Therefore, I use forbes_data
# Step 1: compute the sum of net_worth per country.
country_worth <- forbes_data %>%
  # Group by country
  group_by(country) %>%
  # Make new data - sum of net_worth per country
  summarize(
    sum(net_worth)
  ) %>%
  # Rename new column
  rename(
    country_worth = "sum(net_worth)"
  )

# Step 2: Plot this sum on the world map.
