

# Preliminaries:
rm(list = ls())
library(tidyverse)
library(matrixStats) # needed to calculate weighted median
library(scales)
library(ggpubr)
theme_set(theme_light())

# Load data:
data <- read_csv("RA_21_22.csv")

# Inspect data:
glimpse(data)



# 1. Please summarize key trends in median total wealth over the last 30 years
# by race and education using plots and in writing.


# Function to summarize by groups:
summ_by <- function(var1, var2 = NULL) {
  require(matrixStats)

  tbl <- data %>%
    mutate(wealth_total = asset_total - debt_total) %>%
    group_by(year, {{ var1 }}, {{ var2 }}) %>%
    summarise(median_wealth = weightedMedian(wealth_total, weight))

  return(tbl)
}

# Use function to summarize data:
by_race <- summ_by(race)
by_educ <- summ_by(education)
by_race_educ <- summ_by(race, education)

# Function to produce time series plots:
plot_by <- function(tbl, y = median_wealth, by = race,
                    brks = c("white", "other", "Hispanic", "black"),
                    labs = c("Whites", "Other", "Hispanic", "Black")) {
  
  require(ggplot2); require(scales)
  
  p <- tbl %>%
    ggplot(aes(year, {{ y }}, color = {{ by }}, linetype = {{ by }})) +
    geom_line(size = 0.85) +
    scale_y_continuous(labels = label_number_si()) +
    scale_color_discrete(breaks = brks, labels = labs) +
    scale_linetype_discrete(breaks = brks, labels = labs) +
    scale_x_continuous(
      # "count()" of "dplyr" is masked by "matrixStats"
      breaks = dplyr::count(data, year) %>% pull(year) %>% .[c(2, 6, 10)]
    ) +
    theme(legend.position = "top") +
    labs(x = "Year", y = "Median Wealth (2016 dollars)", 
         color = "", linetype = "")

  return(p)
}

# Figure 1
plot_by(by_race)

# Figure 2:
plot_by(
  by_educ,
  by = education,
  brks = c("college degree", "some college", "no college"),
  labs = c("College Degree", "Some College", "No College")
)

# Figure 3:
plot_by(by_race_educ) +
  facet_wrap(~education, labeller = as_labeller(
    c(
      "college degree" = "College Degree",
      "some college" = "Some College",
      "no college" = "No College"
    )
  ))

# Figure 4:
data %>%
  filter(race %in% c("white", "black")) %>%
  mutate(wealth_housing = asset_housing - debt_housing) %>%
  group_by(year, race) %>%
  summarise(median_housing = weightedMedian(wealth_housing, weight)) %>%
  plot_by(
    by = race,
    y = median_housing,
    brks = c("white", "black"),
    labs = c("Whites", "Blacks")
  )


# 3. Many households are not homeowners and so your analysis for the prior question includes many zeros
# for housing wealth. Let’s dig deeper by focusing just on homeowners age 25 or older. Subsetting to
# homeowners age 25 or older, please summarize trends in median housing and non-housing wealth for
# black and white households. Which group had the largest loss in housing wealth, where 2007 is defined
# as the base period? Please answer this question both in dollar terms and in proportional terms.


# Calculate Median Wealth:
owners_above_25 <- data %>%
  mutate(
    wealth_total = asset_total - debt_total,
    wealth_housing = asset_housing - debt_housing,
    wealth_non_housing = wealth_total - wealth_housing
  ) %>%
  ## subset: older than 25, blacks and whites and owners of houses
  filter(age >= 25, race %in% c("white", "black"), wealth_housing > 0) %>%
  group_by(year, race) %>%
  summarise(
    median_housing = weightedMedian(wealth_housing, weight),
    median_non_housing = weightedMedian(wealth_non_housing, weight)
  ) %>%
  pivot_longer(
    starts_with("median"),
    names_to = "wealth", values_to = "median"
  ) %>%
  mutate(wealth = str_remove(wealth, "median_"))

# Figure 5:
owners_above_25 %>%
  ggplot(aes(factor(year), median, fill = race)) +
  geom_col(position = "dodge") +
  facet_wrap(
    ~wealth,
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
    fill = "" 
  )


# Compute absolute and proportional change:
change_2007 <- owners_above_25 %>%
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

# Plot absolute change:
change_abs <- change_2007 %>%
  filter(scale == "abs") %>%
  ggplot(aes(year, change, fill = race)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  expand_limits(y = 10000) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_discrete(
    breaks = c("white", "black"),
    labels = c("Whites", "Blacks")
  ) +
  theme(legend.position = "top") +
  labs(
    x = "Year", y = "Median Housing Wealth\n (2016 dollars)", fill = ""
  )

# Plot percentage change:
change_pct <- change_2007 %>%
  filter(scale == "pct") %>%
  ggplot(aes(year, change, fill = race)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  expand_limits(y = c(-0.25, 0.05)) +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(breaks = c("white", "black"), 
                      labels = c("Whites", "Blacks")) +
  theme(legend.position = "top") +
  labs(x = "Year", y = "Percent", fill = "")

# Gather the two plots in the same figure to get Figure 6:
ggarrange(
  change_abs, change_pct,
  nrow = 1, common.legend = TRUE, legend = "top"
)
