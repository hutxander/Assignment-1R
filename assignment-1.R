library(tidyverse)

# data_analysis_1 ---------------------------------------------------------
forbes_data <- read_csv(
  "forbes.csv",
  # Fix missing values in age
  na = c("-"),
  col_types = cols(
    age = col_integer()
  )
) %>%
  # We change rank and net_worth to numerics outside of read_csv
  # This is because of additional work to be done (taking care of #, $, B/M, digits.)
  # rank first
  separate(
    col = "rank",
    into = c(NA, "rank")
  ) %>%
  mutate(
    rank = as.integer(rank)
  ) %>%
  # Now net_worth
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
    #net_worth = as.integer(net_worth),
    BM = NULL
  )  
  

# data_analysis_2 ---------------------------------------------------------
# Note: this only works if forbes_data is in Environment
forbes_data <- filter(
  forbes_data,
  net_worth >= 1000000000 
)


# data_analysis_3 ---------------------------------------------------------
# Note: this only works if forbes_data is in Environment
ggplot(data = forbes_data, mapping = aes(x = age, y = net_worth)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = forbes_data, mapping = aes(x = age, y = log(net_worth))) +
  geom_point() +
  geom_smooth(se = FALSE)
