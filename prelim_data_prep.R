
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

#### Preliminary: Data preparation ####

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

# (include child number)
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


# Load saved data ####
data <- read_csv("data.csv")

# Get the 2+ and 3+ sample
gt2_sample0 <- data %>%
  filter(firstborn_age %>% between(6, 18)) %>%
  group_by(moth_no) %>%
  arrange(child_dob, .by_group = TRUE) %>%
  mutate(
    birth_order = row_number(child_dob),
    no_kids = n()
  ) %>%
  ungroup() %>%
  filter(no_kids >= 2)

gt3_sample0 <- gt2_sample0 %>%
  filter(no_kids >= 3) %>%
  filter(secondborn_age %>% between(6, 18))

# Generate boy12 and girl12 separately
parity_gt2 <- gt2_sample0 %>%
  select(moth_no, child_dob, birth_order, no_kids, child_sex) %>%
  # extend the vector up to 4 to get the 3+ sample
  filter(birth_order %in% c(1, 2, 3)) %>%
  pivot_wider(
    id_cols = moth_no,
    names_from = birth_order,
    values_from = c(child_sex, child_dob)
  ) %>%
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
  select(-contains(c("child_sex", "child_dob")))

# Being lazy and copying the code:
parity_gt3 <- gt3_sample0 %>%
  select(moth_no, child_dob, birth_order, no_kids, child_sex) %>%
  # extend the vector up to 4 to get the 3+ sample
  filter(birth_order %in% c(1, 2, 3, 4)) %>%
  pivot_wider(
    id_cols = moth_no,
    names_from = birth_order,
    values_from = c(child_sex, child_dob)
  ) %>%
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
  select(-contains(c("child_sex", "child_dob")))

# Should definitely write a function
# parity <- function(variables) {
#
# }
# to be continued...

gt2_sample <- gt2_sample0 %>%
  left_join(parity_gt2, by = "moth_no") %>%
  filter(birth_order == 1) %>%
  # Filter out twins at first birth (and unrealistic obs.)
  filter(!(twins_1 == 1), no_kids < 10) %>%
  mutate(boy = case_when(child_sex == 1 ~ 1, child_sex == 2 ~ 0)) %>%
  select(
    moth_no:child_sex, boy, birth_order:twins_2, no_kids,
    everything(), -(firstborn_dob:secondborn_age)
  )

# Do the same for the 3+ sample
gt3_sample <- gt3_sample0 %>%
  left_join(parity_gt3, by = "moth_no") %>%
  filter(birth_order %in% c(1, 2)) %>%
  filter(!(twins_1 == 1 | twins_2 == 1)) %>%
  filter(no_kids < 10) %>%
  mutate(boy = case_when(child_sex == 1 ~ 1, child_sex == 2 ~ 0)) %>%
  select(
    moth_no:child_sex, boy, birth_order:twins_2, twins_3, no_kids,
    everything(), -(firstborn_dob:secondborn_age)
  )

# rm(list = c("firstborn_dob", "secondborn_dob", "data"))

rm(list = c("parity_gt2", "parity_gt3", "gt2_sample0", "gt3_sample0"))

# sanity check
# semi_join(x, y) keeps all observations in x that have a match in y:
gt3_sample %>%
  semi_join(gt2_sample, by = "moth_no")

# anti_join(x, y) drops all observations in x that have a match in y:
gt3_sample %>%
  anti_join(gt2_sample, by = "moth_no")

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

# clean data (get rid of missing values)

gt2_analysis_sample <- gt2_sample %>%
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>%
  select(-fath_employ, -moth_employ)

# force 3+ to be a sub-sample of 2+
gt3_analysis_sample <- gt3_sample %>%
  filter(!is.na(moth_dob)) %>%
  select(-fath_employ, -moth_employ) %>%
  semi_join(gt2_analysis_sample, by = "moth_no")

# sanity check
# semi_join(x, y) keeps all observations in x that have a match in y:
gt3_analysis_sample %>%
  semi_join(gt2_analysis_sample, by = "moth_no")

# anti_join(x, y) drops all observations in x that have a match in y:
gt3_analysis_sample %>%
  anti_join(gt2_analysis_sample, by = "moth_no")

# other checks:
gt2_analysis_sample %>%
  summarise_all(~ sum(is.na(.)))

gt3_analysis_sample %>%
  summarise_all(~ sum(is.na(.)))

gt3_analysis_sample %>%
  count(moth_no) %>%
  count(n, name = "count") # Brilliant

gt3_analysis_sample %>%
  count(birth_order)


# Do we want to have only hhs with both the first and second born complete?

## Generate (mutate) variables ####

## 2+ sample

# Dummy for private school attendance & child sex (factor)
gt2_analysis_sample <- gt2_analysis_sample %>%
  # Toggle to include 9 (affects sample size by a lot)
  filter(child_private %in% c(1, 2, 9)) %>%
  mutate(private_school = case_when(
    child_private == 2 ~ 1, TRUE ~ 0
  )) %>%
  mutate(child_sex = case_when(
    child_sex == 1 ~ "Male", child_sex == 2 ~ "Female"
  ) %>% factor())

# constructing educational attainment variable
# (There could be outliers for this variable, please check in the future.)
gt2_analysis_sample <- gt2_analysis_sample %>%
  filter(child_educ %in% 0:12 | child_educ == 98) %>%
  mutate(
    child_educ_gen = case_when(
      as.numeric(child_educ) == 98 ~ 1,
      TRUE ~ as.numeric(child_educ) + 2
    )
  ) %>%
  group_by(child_age_year, boy) %>%
  mutate(mean_educ_age_sex = mean(child_educ_gen)) %>%
  ungroup() %>%
  mutate(educ_attain = child_educ_gen / mean_educ_age_sex)

# ? (dummy for "lagged behind")


# Mothers' LFP
gt2_analysis_sample <- gt2_analysis_sample %>%
  mutate(
    moth_lfp_offic = case_when(
      moth_employ_official %in% c(1, 2) ~ 1,
      moth_employ_official %in% c(3, 4) ~ 0
    ),
    moth_lfp_ext = case_when(
      moth_employ_extended %in% c(1, 2) ~ 1,
      moth_employ_extended == 3 ~ 0
    )
  ) %>%
  filter(!is.na(moth_employ_official))

# Mothers' population group
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

## 3+ sample

# Dummy for private school attendance & child sex (factor)
gt3_analysis_sample <- gt3_analysis_sample %>%
  # Toggle to include 9 (affects sample size by a lot)
  filter(child_private %in% c(1, 2, 9)) %>%
  mutate(private_school = case_when(
    child_private == 2 ~ 1, TRUE ~ 0
  )) %>%
  mutate(child_sex = case_when(
    child_sex == 1 ~ "Male", child_sex == 2 ~ "Female"
  ) %>% factor())

# constructing educational attainment variable
# (There could be outliers for this variable, please check in the future.)
gt3_analysis_sample <- gt3_analysis_sample %>%
  filter(child_educ %in% 0:12 | child_educ == 98) %>%
  mutate(
    child_educ_gen = case_when(
      as.numeric(child_educ) == 98 ~ -1,
      TRUE ~ as.numeric(child_educ)
    )
  ) %>%
  group_by(child_age_year, boy) %>%
  mutate(mean_educ_age_sex = mean(child_educ_gen)) %>%
  ungroup() %>%
  mutate(educ_attain = child_educ_gen / mean_educ_age_sex)

# For the rest, join with 2+ sample
gt3_analysis_sample <- gt3_analysis_sample %>%
  semi_join( # d/ce with "left_join()" is only 87 obs.
    gt2_analysis_sample %>%
      select(moth_no, moth_lfp_offic, moth_lfp_ext, moth_pp_group_fct),
    by = "moth_no"
  )


# Good idea to save the analysis files:
write_csv(gt2_analysis_sample, "./gt2_analysis_sample.csv")
write_csv(gt3_analysis_sample, "./gt3_analysis_sample.csv")


# Focus in on child education
gt2_analysis_sample %>% 
  count(child_educ)

gt2_analysis_sample %>% 
  filter(child_educ %in% c(0, 98)) %>% 
  count(child_age_year, child_educ)

# Grade 0 are currently Grade 1 [see p. 63 in sample meta data]
# 98 are currently Grade 0 (all were attending school)

# Problematic with the zeros in "child_educ_gen"!
gt2_analysis_sample %>% 
  group_by(child_educ_gen) %>% 
  summarise(mean_0 = mean(mean_educ_age_sex),
            sd_0 = sd(mean_educ_age_sex),
            mean_1 = mean(educ_attain),
            sd_1 = sd(educ_attain))


gt2_analysis_sample %>% 
  ggplot(aes(educ_attain)) +
  geom_histogram()

gt2_analysis_sample %>% 
  ggplot(aes(factor(child_educ_gen), educ_attain)) +
  geom_boxplot()

gt2_analysis_sample %>% 
  filter(child_educ_gen == -1) %>% 
  select(child_educ_gen, mean_educ_age_sex, educ_attain)

# Tentative solution: add 2 to all grades (so it ranges from 1 to 14)







