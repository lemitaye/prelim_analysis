
### Preliminary analysis ###

# created on: 2/15/2022

rm(list = ls())
# setwd("D:/Doc/paper/Census_2011")

library(haven)
library(tidyverse)
library(janitor)
library(lubridate)
library(AER)
library(stargazer)
library(scales)
library(broom)
library(car)

theme_set(theme_light())


# Load saved data
gt2_analysis_sample <- read_csv("gt2_analysis_sample.csv")
gt3_analysis_sample <- read_csv("gt3_analysis_sample.csv")

# First-stage regressions ####

m1 <- lm(no_kids ~ same_sex_12, data = gt2_analysis_sample)
m2 <- lm(no_kids ~ boy_1 + boy_2 + same_sex_12, data = gt2_analysis_sample)
m3 <- lm(no_kids ~ boy_1 + boy_12 + girl_12, data = gt2_analysis_sample)
m4 <- lm(no_kids ~ twins_2, data = gt2_analysis_sample)
stargazer(m1, m2, m3, m4, type = "text", keep.stat = c("n","rsq"))

# F-tests (suspected weak instruments)
linearHypothesis(m1, c("same_sex_12"))
linearHypothesis(m2, c("boy_1", "boy_2", "same_sex_12"))
linearHypothesis(m3, c("boy_1", "boy_12", "girl_12"))
linearHypothesis(m4, c("twins_2"))


# Strong first-stage. Looks good so far.

# Angrist et al. (2010) cluster the standard errors for the regressions using
# the 3+ sample by mohter's ID. (see p. 791)
m5 <- lm(no_kids ~ same_sex_123 + boy, data = gt3_analysis_sample)
m6 <- lm(no_kids ~ boy_123 + girl_123 + boy_3:I(1-same_sex_12), 
         data = gt3_analysis_sample)
m7 <- lm(no_kids ~ twins_3, data = gt3_analysis_sample)
stargazer(m5, m6, m7, type = "text", keep.stat = c("n","rsq"))



# 2SLS/IV regressions ####

## 2+ sample ####
# (I think the OLS results are Wald estimates) 
OLS_A1 <- lm(educ_attain ~ no_kids, data = gt2_analysis_sample)
IV_A1 <- ivreg(educ_attain ~ no_kids | 
                 twins_2, data = gt2_analysis_sample)
IV_A2 <- ivreg(educ_attain ~ no_kids | 
                 same_sex_12, data = gt2_analysis_sample)
IV_A3 <- ivreg(educ_attain ~ no_kids | 
                 boy_12 + girl_12, data = gt2_analysis_sample)
IV_A4 <- ivreg(educ_attain ~ no_kids | 
                 twins_2 + boy_12 + girl_12, data = gt2_analysis_sample)
stargazer(OLS_A1, IV_A1, IV_A2, IV_A3, IV_A4, 
          type = "text", keep.stat = c("n","rsq"))

OLS_A2 <- lm(private_school ~ no_kids, data = gt2_analysis_sample)
IV_A5 <- ivreg(private_school ~ no_kids | 
                 twins_2, data = gt2_analysis_sample)
IV_A6 <- ivreg(private_school ~ no_kids | 
                 same_sex_12, data = gt2_analysis_sample)
IV_A7 <- ivreg(private_school ~ no_kids | 
                 boy_12 + girl_12, data = gt2_analysis_sample)
IV_A8 <- ivreg(private_school ~ no_kids | 
                 twins_2 + boy_12 + girl_12, data = gt2_analysis_sample)
stargazer(OLS_A2, IV_A5, IV_A6, IV_A7, IV_A8, 
          type = "text", keep.stat = c("n","rsq"))

# Tried both official and extended versions of female LFP
OLS_A3 <- lm(moth_lfp_ext ~ no_kids, data = gt2_analysis_sample)
IV_A9 <- ivreg(moth_lfp_ext ~ no_kids | 
                 twins_2, data = gt2_analysis_sample)
IV_A10 <- ivreg(moth_lfp_ext ~ no_kids | 
                  same_sex_12, data = gt2_analysis_sample)
IV_A11 <- ivreg(moth_lfp_ext ~ no_kids | 
                  boy_12 + girl_12, data = gt2_analysis_sample)
stargazer(OLS_A3, IV_A9, IV_A10, IV_A11, 
          type = "text", keep.stat = c("n","rsq"))


# Next steps: do the same for the 3+ sample, construct and include 
# covariates, and start heterogeneity analysis (this is going to be fun!)
# Discuss exclusion restrictions
# Educ-attain and private school need to be revised

## 3+ sample ####

# Educational attainment
OLS_B1 <- lm(educ_attain ~ no_kids, data = gt3_analysis_sample)
IV_B1 <- ivreg(educ_attain ~ no_kids | 
                 twins_3, data = gt3_analysis_sample)
IV_B2 <- ivreg(educ_attain ~ no_kids | 
                 same_sex_123, data = gt3_analysis_sample)
IV_B3 <- ivreg(educ_attain ~ no_kids | 
                 boy_123 + girl_123, data = gt3_analysis_sample)
IV_B4 <- ivreg(educ_attain ~ no_kids | 
                 twins_3 + boy_123 + girl_123, data = gt3_analysis_sample)
stargazer(OLS_B1, IV_B1, IV_B2, IV_B3, IV_B4, 
          type = "text", keep.stat = c("n","rsq"))

# Private school attendance
OLS_B2 <- lm(private_school ~ no_kids, data = gt3_analysis_sample)
IV_B5 <- ivreg(private_school ~ no_kids | 
                 twins_3, data = gt3_analysis_sample)
IV_B6 <- ivreg(private_school ~ no_kids | 
                 same_sex_123, data = gt3_analysis_sample)
IV_B7 <- ivreg(private_school ~ no_kids | 
                 boy_123 + girl_123, data = gt3_analysis_sample)
IV_B8 <- ivreg(private_school ~ no_kids | 
                 twins_3 + boy_123 + girl_123, data = gt3_analysis_sample)
stargazer(OLS_B2, IV_B5, IV_B6, IV_B7, IV_B8, 
          type = "text", keep.stat = c("n","rsq"))

# Mothers' LFP (Try both official and extended versions of female LFP)
OLS_B3 <- lm(moth_lfp_ext ~ no_kids, data = gt3_analysis_sample)
IV_B7 <- ivreg(moth_lfp_ext ~ no_kids | twins_3, data = gt3_analysis_sample)
IV_B8 <- ivreg(moth_lfp_ext ~ no_kids | same_sex_123, data = gt3_analysis_sample)
IV_B9 <- ivreg(moth_lfp_ext ~ no_kids | boy_123 + girl_123, data = gt3_analysis_sample)
stargazer(OLS_B3, IV_B7, IV_B8, IV_B9, type = "text", keep.stat = c("n","rsq"))

# Consider including region fixed effects.




# Some graphics ####

# Proportion of number of siblings
gt2_analysis_sample %>% 
  count(moth_pp_group, no_kids) %>% 
  filter(moth_pp_group != 5) %>% 
  group_by(moth_pp_group) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(factor(no_kids), prop)) + 
  geom_col(mapping = aes(fill = moth_pp_group), position = "dodge") + 
  # facet_wrap(~moth_pp_group) +
  scale_y_continuous(label = percent)


gt2_analysis_sample %>% 
  count(moth_pp_group_fct, no_kids) %>% 
  # filter(moth_pp_group_fct != "Other") %>% 
  mutate(no_kids = if_else(no_kids %in% 5:9, "5+", as.character(no_kids)),
         no_kids = factor(no_kids) %>% fct_rev()) %>% 
  group_by(moth_pp_group_fct) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(no_kids, prop)) +
  geom_col(mapping = aes(fill = moth_pp_group_fct), position = "dodge") + 
  coord_flip() +
  scale_y_continuous(label = percent) + 
  labs(
    title = "Total Number of Children < 18 years old by Population Group",
    x = "Total Number of Children",
    y = "Percent",
    fill = "Mother's Population Group"
  )

my_sum <- gt2_analysis_sample %>% 
  mutate(twins_2 = factor(twins_2)) %>% 
  group_by(twins_2, moth_pp_group_fct) %>% 
  summarise(
    n = n(),
    mean_no_kids = mean(no_kids),
    sd=sd(no_kids)
  ) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(ic = se*qt(.975, n-1))

my_sum %>% 
  filter(moth_pp_group_fct != "Other") %>% 
  ggplot() +
  geom_col(aes(twins_2, mean_no_kids), fill = "forestgreen", alpha = 0.7) + 
  geom_errorbar(aes(x = twins_2, ymin = mean_no_kids - sd,
                    ymax = mean_no_kids + sd), size = 0.5, width = 0.3) +
  facet_wrap(~moth_pp_group_fct) + 
  labs(
    title = "Twins-2 First-Stage by Mothers' Population Group",
    x = "Twins in the Second Birth",
    y = "Average Number of Children"
  )

gt2_analysis_sample %>% 
  mutate(child_age = factor(child_age_year)) %>% 
  select(child_age, child_sex, child_educ_gen) %>% 
  ggplot(aes(child_age, child_educ_gen)) +
  geom_boxplot(aes(fill = child_sex)) +
  labs(
    title = "Age and Years of Schooling",
    x = "Age of Child",
    y = "Years of Schooling",
    fill = "Sex",
    caption = "Note: Years of Schooling is based on author's calculation"
  )

gt2_analysis_sample %>%
  mutate(no_kids = if_else(no_kids %in% 5:9, "5+", as.character(no_kids)),
         no_kids = factor(no_kids) ) %>% 
  count(no_kids, child_sex, private_school) %>% 
  group_by(no_kids) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(no_kids, prop)) +
  geom_col(mapping = aes(fill = child_sex), position = "dodge") + 
  # coord_flip() +
  scale_y_continuous(label = percent) + 
  labs(
    title = "Private School Attendance by Number of Kids and Sex",
    x = "Total Number of Children",
    y = "Percent",
    fill = "Sex"
  )

# Reduced form effect on private school attendance using both "same sex" 
# and "twins_2"

simple_model_twins <- function(tbl) {
  lm(private_school ~ twins_2, data = tbl)
}

simple_model_samesex <- function(tbl) {
  lm(private_school ~ same_sex_12, data = tbl)
}

get_confs <- function(mod) {
  confint(mod) %>% 
    as_tibble()
}


simple_mod_data_samesex <- gt2_analysis_sample %>%
  mutate(no_kids = if_else(no_kids %in% 5:9, "5+", as.character(no_kids)),
         no_kids = factor(no_kids, levels = c("2", "3", "4", "5+")) ) %>% 
  select(private_school, no_kids, child_sex, twins_2, same_sex_12) %>% 
  group_by(no_kids, child_sex) %>% 
  nest() %>% 
  mutate(
    model = map(data, simple_model_samesex ), 
    summaries = map(model, tidy), 
    conf_ints = map(model, get_confs)
  ) %>% 
  unnest(c(summaries, conf_ints)) %>% 
  rename("conf_low" = `2.5 %`, "conf_high" = `97.5 %`) %>% 
  arrange(no_kids, child_sex)

simple_mod_data_samesex %>% 
  filter(term == "same_sex_12") %>% 
  ggplot(aes(fct_rev(no_kids), estimate)) + 
  geom_point(color = "red") + 
  facet_wrap(~ child_sex, scales = "free_y") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = .1 ) +
  geom_hline(aes(yintercept = 0)) +
  coord_flip() +
  labs(
    title = "Plot of Simple Linear Regression Coefficient",
    x = "Total Number of Children",
    y = "Coefficient",
    caption = "The red dots indicate coefficient estimate and the lines are 95% confidence intervals"
  )


# 2/11/2022

# Run test for weak instruments (probably need to use stata)
# Need to revise measurement of "educational attainment"
# Can the school starting age be different in different districts? (see Delpiano, 2006)
