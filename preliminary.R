
# Created on Dec. 16, 2021

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

#### Preliminary ####

# Append the two data sets (memory intensive)
gc()
memory.limit(9999999999)
person_tot <- read_dta("sa-census-2011-person-all-v1.2.dta")
gc()
  
person_tot %>% 
  glimpse()

person_tot <- person_tot %>% 
  clean_names() %>% 
  filter(qn_type == 1) %>% 
  mutate(dob = make_date(p01_year, p01_month, p01_day)) %>% 
  # the census was conducted on 9-10 October, 2011
  mutate(
    age_month = interval(dob, ymd(20111010)) %/% months(1),
    age_year = interval(dob, ymd(20111010)) %/% years(1)
  ) %>% 
  select(sn, f00_nr, dob, f03_sex, age_month, age_year, everything())

person_tot %>% 
  glimpse()


# Siblings are kids who share the same biological mother in a household

kids <- person_tot %>% 
  # filter(between(age_year, 6, 18)) %>% # the firstborn could be >18yrs old
  filter(p14_motheralive == 1 & p15_fatheralive == 1) %>%
  mutate(
    moth_no = str_c(sn, p14a_motherpnr),
    fath_no = str_c(sn, p15a_fatherpnr)
    ) %>% 
  select(
    # other variables need to be added (not final list)
    moth_no,
    fath_no,
    child_dob = dob, 
    child_sex = f03_sex,
    child_age_year = age_year,
    child_age_month = age_month,
    child_sch_attend = p17_schoolattend,
    child_educ = p20_edulevel,
    child_private = p19_edupubpriv,
    child_pop_group = p05_pop_group
    ) 

mothers <- person_tot %>% 
  mutate(moth_no = str_c(sn, f00_nr)) %>% 
  semi_join(kids, by = "moth_no") %>% 
  # other variables need to be added (not final list)
  select(
    moth_no,
    moth_age_year = age_year,
    moth_age_month = age_month,
    moth_dob = dob,
    moth_marital = p03_marital_st,
    moth_pp_group = p05_pop_group,
    moth_educ = p20_edulevel,
    moth_ceb = p32_childeverborn,
    moth_age_fstbr = p33_agefirstbirth,
    moth_employ = derp_employ_status,
    moth_employ_official = derp_employ_status_official,
    moth_employ_extended = derp_employ_status_expanded
         )

fathers <- person_tot %>% 
  mutate(fath_no = str_c(sn, f00_nr)) %>% 
  semi_join(kids, by = "fath_no") %>% 
  # other variables need to be added (not final list)
  select(
    fath_no,
    fath_age_year = age_year,
    fath_age_month = age_month,
    fath_dob = dob,
    fath_marital = p03_marital_st,
    fath_pp_group = p05_pop_group,
    fath_educ = p20_edulevel,
    fath_employ = derp_employ_status,
    fath_employ_official = derp_employ_status_official,
    fath_employ_extended = derp_employ_status_expanded
  )

data <- kids %>% 
  left_join(mothers, by = "moth_no") %>% 
  left_join(fathers, by = "fath_no")

# It is time to free-up some memory
rm(list = c("fathers", "mothers", "kids", "person_tot"))

# Construct birth order (most time consuming code)
# data <- data %>% 
#   group_by(moth_no) %>%
#   arrange(moth_no, child_dob) %>% 
#   mutate(birth_order = row_number(child_dob)) %>% # this could be done later
#   ungroup(first_born_dob = "child_dob")

# Get the dob of the first-born (time consuming)
firstborn_dob <- data %>% 
  select(moth_no, child_dob) %>% 
  group_by(moth_no) %>% 
  arrange(child_dob, .by_group = TRUE) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(firstborn_dob = "child_dob") %>% 
  mutate(firstborn_age = interval(firstborn_dob, ymd(20111010)) %/% years(1))

secondborn_dob <- data %>% 
  select(moth_no, child_dob) %>% 
  group_by(moth_no) %>% 
  arrange(child_dob, .by_group = TRUE) %>% 
  slice(2) %>% 
  ungroup() %>% 
  rename(secondborn_dob = "child_dob") %>% 
  mutate(secondborn_age = interval(secondborn_dob, ymd(20111010)) %/% years(1))

# Thinking of writing a function for the above two...
get_dob <- function(tbl, order, new_name) {
  tbl %>% 
    select(moth_no, child_dob) %>% 
    group_by(moth_no) %>% 
    arrange(child_dob, .by_group = TRUE) %>% 
    slice(2) %>% 
    ungroup() %>% 
    rename(secondborn_dob = "child_dob") %>% 
    mutate(secondborn_age = interval(secondborn_dob, ymd(20111010)) %/% years(1))
  
}

# firstborn_dob <- firstborn_dob %>%   
#   ungroup() %>% 
#   mutate(firstborn_age = interval(firstborn_dob, ymd(20111001)) %/% years(1))

data <- data %>% 
  left_join(firstborn_dob, by = "moth_no") %>% 
  left_join(secondborn_dob, by = "moth_no") %>% 
  filter(firstborn_age <= 18)

# save data
write_csv(data, path = "./data.csv")


#### Load saved data ####
data <- read_csv("data.csv")

# Get the 2+ and 3+ sample
gt2_sample0 <- data %>% 
  filter( firstborn_age %>% between(6, 18) ) %>% 
  group_by(moth_no) %>% 
  arrange(child_dob, .by_group = TRUE) %>% 
  mutate(birth_order = row_number(child_dob),
         no_kids = n() ) %>%
  ungroup() %>% 
  filter(no_kids >= 2) 

gt3_sample0 <- gt2_sample0 %>% 
  filter( no_kids >= 3 ) %>% 
  filter( secondborn_age %>% between(6, 18) )

# Generate boy12 and girl12 separately
parity_gt2 <- gt2_sample0 %>% 
  select(moth_no, child_dob, birth_order, no_kids, child_sex) %>% 
  # extend the vector up to 4 to get the 3+ sample
  filter(birth_order %in% c(1, 2, 3)) %>% 
  pivot_wider(id_cols = moth_no, 
              names_from = birth_order, 
              values_from = c(child_sex, child_dob) ) %>% 
  mutate(
    boy_1 = case_when(
      child_sex_1 == 1 ~ 1,
      TRUE ~ 0
    ),
    boy_2 = case_when(
      child_sex_2 == 1 ~ 1,
      TRUE ~ 0
    ),
    boy_12 = case_when(
      (child_sex_1 == 1 & child_sex_2 == 1) ~ 1,
      TRUE ~ 0
    ),
    girl_12 = case_when(
      (child_sex_1 == 2 & child_sex_2 == 2) ~ 1,
      TRUE ~ 0
    ),
    same_sex_12 = boy_12 + girl_12,
    # The following commented out part is relevant for the 3+ sample
    # boy_123 = case_when(
    #   (child_sex_1 == 1 & child_sex_2 == 1 & child_sex_3 == 1) ~ 1,
    #   TRUE ~ 0
    # ),
    # girl_123 = case_when(
    #   (child_sex_1 == 2 & child_sex_2 == 2 & child_sex_3 == 2) ~ 1,
    #   TRUE ~ 0
    # ),
    # and for the twins:
    twins_1 = case_when(
      (child_dob_1 == child_dob_2) ~ 1,
      TRUE ~ 0
    ),
    twins_2 = case_when(
      (child_dob_2 == child_dob_3) ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select( -contains( c("child_sex", "child_dob") ) )

# Being lazy and copying the code:
parity_gt3 <- gt3_sample0 %>% 
  select(moth_no, child_dob, birth_order, no_kids, child_sex) %>% 
  # extend the vector up to 4 to get the 3+ sample
  filter(birth_order %in% c(1, 2, 3, 4)) %>% 
  pivot_wider(id_cols = moth_no, 
              names_from = birth_order, 
              values_from = c(child_sex, child_dob) ) %>% 
  mutate(
    boy_1 = case_when(
      child_sex_1 == 1 ~ 1,
      TRUE ~ 0
    ),
    boy_2 = case_when(
      child_sex_2 == 1 ~ 1,
      TRUE ~ 0
    ),
    boy_3 = case_when(
      child_sex_3 == 1 ~ 1,
      TRUE ~ 0
    ),
    boy_12 = case_when(
      (child_sex_1 == 1 & child_sex_2 == 1) ~ 1,
      TRUE ~ 0
    ),
    girl_12 = case_when(
      (child_sex_1 == 2 & child_sex_2 == 2) ~ 1,
      TRUE ~ 0
    ),
    same_sex_12 = boy_12 + girl_12,
    # The following commented out part is relevant for the 3+ sample
    boy_123 = case_when(
      (child_sex_1 == 1 & child_sex_2 == 1 & child_sex_3 == 1) ~ 1,
      TRUE ~ 0
    ),
    girl_123 = case_when(
      (child_sex_1 == 2 & child_sex_2 == 2 & child_sex_3 == 2) ~ 1,
      TRUE ~ 0
    ),
    same_sex_123 = boy_123 + girl_123,
    # and for the twins:
    twins_1 = case_when(
      (child_dob_1 == child_dob_2) ~ 1,
      TRUE ~ 0
    ),
    twins_2 = case_when(
      (child_dob_2 == child_dob_3) ~ 1,
      TRUE ~ 0
    ),
    twins_3 = case_when(
      (child_dob_3 == child_dob_4) ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select( -contains( c("child_sex", "child_dob") ) )

# Should definitely write a function
# parity <- function(variables) {
#   
# }
# to be continued...

gt2_sample <- gt2_sample0 %>% 
  left_join(parity_gt2, by = "moth_no") %>% 
  filter(birth_order == 1) %>% 
  # Filter out twins at first birth (and unrealistic obs.)
  filter( !(twins_1 == 1), no_kids < 10 ) %>% 
  mutate( boy = case_when( child_sex == 1 ~ 1, child_sex == 2 ~ 0 ) ) %>% 
  select(moth_no:child_sex, boy, birth_order:twins_2, no_kids,
         everything(), -(firstborn_dob:secondborn_age) ) 

# Do the same for the 3+ sample
gt3_sample <- gt3_sample0 %>% 
  left_join(parity_gt3, by = "moth_no") %>% 
  filter(birth_order %in% c(1, 2)) %>%
  filter(!(twins_1==1 | twins_2==1)) %>% 
  filter(no_kids < 10) %>% 
  mutate( boy = case_when( child_sex == 1 ~ 1, child_sex == 2 ~ 0 ) ) %>% 
  select(moth_no:child_sex, boy, birth_order:twins_2, twins_3, no_kids,
         everything(), -(firstborn_dob:secondborn_age) ) 
  
rm(list = c("firstborn_dob", "secondborn_dob", "data"))

rm(list = c("parity_gt2", "parity_gt3", "gt2_sample0", "gt3_sample0"))
  
# Include father's characteristics x
# Get the 3+ sample x
# Generate age (preferably in months) x

# Both father and mother are present in the dataset
gt2_sample %>% 
  count(moth_no, fath_no)

# Three outcomes I am considering:
# * Highest completed grade (adjusted for age)
# * Private school
# * Mother's LFP 

# Covariates:
# * age - probably using dummies or in months
# * child's sex
# * parents' age
# * parents' education
# * child's and parents' pop. group (probably only mother's)
# * province/state of residence

# clean data (count missing values for all vars)
gt2_sample %>% 
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>% 
  select(-fath_employ,-moth_employ) %>% 
  summarise_all( ~sum(is.na(.)) )

gt3_sample %>% 
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>%
  select(-fath_employ,-moth_employ) %>% count(twins_3)
  summarise_all( ~sum(is.na(.)) )

gt2_analysis_sample <- gt2_sample %>% 
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>% 
  select(-fath_employ,-moth_employ)

gt3_analysis_sample <- gt3_sample %>% 
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>%
  select(-fath_employ,-moth_employ) 


### Generate (mutate) variables

## 2+ sample

# Dummy for private school attendance & child sex (factor)
gt2_analysis_sample <- gt2_analysis_sample %>% 
  # Toggle to include 9 (affects sample size by a lot)
  filter(child_private %in% c(1, 2, 9)) %>% 
  mutate(private_school = case_when(
      child_private == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate( child_sex = case_when(
    child_sex == 1 ~ "Male", child_sex == 2 ~ "Female"
  ) %>% factor() )
 
# constructing educational attainment variable
# (There could be outliers for this variable, please check in the future.)
gt2_analysis_sample <- gt2_analysis_sample %>%  
  filter(child_educ %in% 0:12 | child_educ == 98) %>% 
  mutate(
    child_educ_gen = case_when( as.numeric(child_educ) == 98 ~ -1, 
                                TRUE ~ as.numeric(child_educ) )) %>% 
  group_by(child_age_year, boy) %>% 
  mutate(mean_educ_age_sex = mean(child_educ_gen)) %>% 
  ungroup() %>% 
  mutate(educ_attain = child_educ_gen/mean_educ_age_sex) 

# ? (dummy for "lagged behind")


# Mothers' LFP
gt2_analysis_sample <- gt2_analysis_sample %>% 
  mutate(
    moth_lfp_offic = case_when(
      moth_employ_official %in% c(1, 2) ~ 1, 
      moth_employ_official %in% c(3, 4) ~ 0),
    
    moth_lfp_ext = case_when(
      moth_employ_extended %in% c(1, 2) ~ 1, 
      moth_employ_extended == 3 ~ 0) ) %>% 
  filter(!is.na(moth_employ_official))

gt2_analysis_sample <- gt2_analysis_sample %>% 
  mutate(
    moth_pp_group_fct = 
           case_when(
             moth_pp_group == 1 ~ "Black African",
             moth_pp_group == 2 ~ "Coloured",
             moth_pp_group == 3 ~ "Indian or Asian",
             moth_pp_group == 4 ~ "White",
             moth_pp_group == 5 ~ "Other",
           ) %>% factor()
    ) 

## +3 sample




# Good idea to save the analysis file:
write_csv(gt2_analysis_sample, "./gt2_analysis_sample")


#### Preliminary analysis ####
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

  
# Now, time for 2SLS
 # I think the first two are Wald estimates 
OLS1 <- lm(educ_attain ~ no_kids, data = gt2_analysis_sample)
IV1 <- ivreg(educ_attain ~ no_kids | twins_2, data = gt2_analysis_sample)
IV2 <- ivreg(educ_attain ~ no_kids | same_sex_12, data = gt2_analysis_sample)
IV3 <- ivreg(educ_attain ~ no_kids | boy_12 + girl_12, data = gt2_analysis_sample)
stargazer(OLS1, IV1, IV2, IV3, type = "text", keep.stat = c("n","rsq"))

OLS2 <- lm(private_school ~ no_kids, data = gt2_analysis_sample)
IV4 <- ivreg(private_school ~ no_kids | twins_2, data = gt2_analysis_sample)
IV5 <- ivreg(private_school ~ no_kids | same_sex_12, data = gt2_analysis_sample)
IV6 <- ivreg(private_school ~ no_kids | boy_12 + girl_12, data = gt2_analysis_sample)
stargazer(OLS2, IV4, IV5, IV6, type = "text", keep.stat = c("n","rsq"))

# Tried both official and extended versions of female LFP
OLS3 <- lm(moth_lfp_ext ~ no_kids, data = gt2_analysis_sample)
IV7 <- ivreg(moth_lfp_ext ~ no_kids | twins_2, data = gt2_analysis_sample)
IV8 <- ivreg(moth_lfp_ext ~ no_kids | same_sex_12, data = gt2_analysis_sample)
IV9 <- ivreg(moth_lfp_ext ~ no_kids | boy_12 + girl_12, data = gt2_analysis_sample)
stargazer(OLS3, IV7, IV8, IV9, type = "text", keep.stat = c("n","rsq"))

  
# Next steps: do the same for the +3 sample, construct and include 
# covariates, and start heterogeneity analysis (this is going to be fun!)


# Some graphics: 
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


















