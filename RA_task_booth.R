

library(tidyverse)
library(matrixStats)
library(scales)
theme_set(theme_light())

data <- read_csv("RA_21_22.csv")

glimpse(data)

# 1. Please summarize key trends in median total wealth over the last 30 years
# by race and education using plots and in writing.

summ_by <- function(var) {
  require(matrixStats) # needed to calculate weighted median

  tbl <- data %>%
    mutate(wealth_total = asset_total - debt_total) %>%
    group_by(year, {{ var }}) %>%
    summarise(median_wealth = weightedMedian(wealth_total, weight))

  return(tbl)
}

by_race <- summ_by(race)
by_educ <- summ_by(education)

# Plots:
by_race %>%
  ggplot(aes(year, median_wealth, color = race, linetype = race)) +
  geom_line()

by_educ %>%
  ggplot(aes(year, median_wealth, color = education, linetype = education)) +
  geom_line() +
  scale_y_continuous(labels = label_number_si()) +
  labs(
    x = "Year",
    y = "Median Total Wealth (2016 dollars)",
    color = "Education",
    linetype = "Education"
  )


# We have the expected story: whites and the educated are rich, blacks and the
# uneducated are poor.

# 2.
data %>%
  filter(race %in% c("white", "black")) %>%
  mutate(wealth_housing = asset_housing - debt_housing) %>%
  group_by(year, race) %>%
  summarise(median_housing = weightedMedian(wealth_housing, weight)) %>%
  ggplot(aes(year, median_housing, color = race, linetype = race)) +
  geom_line()


# 3.
above_25 <- data %>%
  filter(age >= 25, race %in% c("white", "black")) %>%
  mutate(
    wealth_total = asset_total - debt_total,
    wealth_housing = asset_housing - debt_housing,
    wealth_non_housing = wealth_total - wealth_housing
  ) %>%
  group_by(year, race) %>%
  summarise(
    median_housing = weightedMedian(wealth_housing, weight),
    median_non_housing = weightedMedian(wealth_non_housing, weight)
  ) %>%
  pivot_longer(starts_with("median"), names_to = "wealth", values_to = "median") %>%
  mutate(wealth = str_remove(wealth, "median_"))

above_25 %>%
  ggplot(aes(factor(year), median, fill = race)) +
  geom_col(position = "dodge") +
  facet_wrap(~wealth,
    nrow = 2,
    labeller = as_labeller(
      c("housing" = "Housing", "non_housing" = "Non-Housing")
    )
  ) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_discrete(
    breaks = c("white", "black"),
    labels = c("Whites", "Blacks")
  ) +
  labs(
    x = "Year",
    y = "Median Wealth (2016 dollars)",
    fill = "Race",
    title = "Median Wealth by Type (Housing vs. Non-Housing) and Race"
  )


# Which group had the largest loss in housing wealth, where 2007 is defined
# as the base year? Please, answer this question both in dollar terms and
# proportional terms.

change_2007 <- above_25 %>%
  filter(year >= 2007, wealth == "housing") %>%
  pivot_wider(names_from = year, values_from = "median") %>%
  mutate(
    loss_2010_abs = `2010` - `2007`,
    loss_2010_pct = (`2010` - `2007`) / `2007`,
    loss_2013_abs = `2013` - `2007`,
    loss_2013_pct = (`2013` - `2007`) / `2007`,
    loss_2016_abs = `2016` - `2007`,
    loss_2016_pct = (`2016` - `2007`) / `2007`
  ) %>%
  select(-c(`2007`:`2016`)) %>%
  pivot_longer(starts_with("loss"), names_to = "loss", values_to = "change") %>%
  separate(loss, into = c("x", "year", "scale"), sep = "_") %>%
  select(-x)

change_2007 %>%
  filter(scale == "abs") %>% 
  ggplot(aes(year, change, fill = race)) +
  geom_col(position = "dodge") +
  expand_limits(y = 1000) +
  scale_y_continuous(labels = label_number_si()) 

change_2007 %>%
  filter(scale == "pct") %>% 
  ggplot(aes(year, change, fill = race)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  expand_limits(y = 0.2) +
  scale_y_continuous(labels = percent) 
  
# how to have different scales for different facets? 



















