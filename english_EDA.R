
# Exploratory Data anlaysis for the project on the influence of
# exposure to english language instruction on cognitive development

# created on: 8/23/2022

rm(list = ls())
setwd("D:/R_projects/Preliminary_Analysis")

# preliminaries
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

# load data
wave1 <- read_dta("data/ethiopia_wave_1.dta")
wave2 <- read_dta("data/ethiopia_wave_2.dta")

# analysis

# merge the two waves and select region, and test scores and
# compare by region

schsurv <- wave1 %>% 
  select(UNIQUEID:REGION_SITE, SCHGRDE, LANG, STDMEAL, 
         STMOMED, STDADED, MTH_TCHLVLEDC, MTH_TCHLVTCTR,
         HTGRDMTH, HTSCHOWN, STDLNGHM, SITE, REGION_SITE,
         starts_with("STLNG"), SCSLNGIN, FCLIBR, RAW_MAT1, 
         RAW_ENG1) %>% 
  left_join(
    wave2 %>% 
      select(UNIQUEID, RAW_MAT2, RAW_ENG2), 
    by = "UNIQUEID") %>% 
  filter(SCHGRDE == 8) %>% 
  clean_names()

# function to standardize test scores
standardize <- function(x){ 
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) 
  return( z)
}

# create variables
schsurv <- schsurv %>% 
  mutate(
    # standardize test scores:
    std_eng_w1 = standardize(raw_eng1),
    std_maths_w1 = standardize(raw_mat1),
    std_eng_w2 = standardize(raw_eng2),
    std_maths_w2 = standardize(raw_mat2),
    
    # name regions:
    region = case_when(
      region == 1 ~ "Addis Ababa",
      region == 2 ~ "Amhara",
      region == 3 ~ "Oromia",
      region == 4 ~ "SNNP",
      region == 5 ~ "Tigray",
      region == 6 ~ "Somali",
      region == 7 ~ "Afar",
    ) %>% factor(),
    
    # section id:
    sect_id = str_c(schlid, sectionid)
    ) 

tigray_oromia <- schsurv %>% 
  filter(region %in% c("Tigray", "Oromia"))

tigray <- schsurv %>% 
  filter(region == "Tigray")

oromia <- schsurv %>% 
  filter(region == "Oromia")

amhara <- schsurv %>% 
  filter(region == "Amhara")

somali <- schsurv %>% 
  filter(region == "Somali")

snnp <- schsurv %>% 
  filter(region == "SNNP")
  

# summmarize by region
by_region <- function(tbl, var) {
  tbl %>% 
    group_by(region) %>% 
    summarise(
      n = n(),
      mean = mean({{var}}, na.rm = TRUE),
      sd = sd({{var}}, na.rm = TRUE)
      ) %>% 
    mutate(se = sd/sqrt(n)) %>% 
    mutate(ic = se*qt(.975, n-1))
}


schsurv %>% 
  filter(htschown == 1) %>% 
  by_region(raw_mat2) %>% 
  mutate(region = fct_reorder(region, mean) %>% fct_rev()) %>% 
  ggplot() +
  geom_col(aes(region, mean), fill = "forestgreen", alpha = 0.7) + 
  geom_errorbar(aes(x = region, ymin = mean - ic,
                    ymax = mean + ic), size = 0.5, width = 0.1) +
  labs(
    title = "Raw Maths Test Score (Wave 2) - Government",
    x = "",
    y = ""
  )

schsurv %>% 
  filter(htschown == 1) %>%
  by_region(std_maths_w2) %>% 
  # filter(region %in% c("Oromia", "SNNP")) %>% 
  mutate(region = fct_reorder(region, mean) %>% fct_rev()) %>% 
  ggplot() +
  geom_col(aes(region, mean), fill = "forestgreen", alpha = 0.7) + 
  geom_errorbar(aes(x = region, ymin = mean - ic,
                    ymax = mean + ic), size = 0.5, width = 0.1) +
  labs(
    title = "Standardized Maths Test Score (Wave 2) - Government",
    x = "Region", y = "", caption = "The bars indicate 95% CI"
  )


by_region(raw_eng2) %>% 
  mutate(region = fct_reorder(region, mean) %>% fct_rev()) %>% 
  ggplot() +
  geom_col(aes(region, mean), fill = "forestgreen", alpha = 0.7) + 
  geom_errorbar(aes(x = region, ymin = mean - ic,
                    ymax = mean + ic), size = 0.5, width = 0.1) +
  labs(
    title = "Raw English Test Score (Wave 2)",
    x = "Region", y = "", caption = "The bars indicate 95% CI"
  )

by_region(std_eng_w2) %>% 
  mutate(region = fct_reorder(region, mean) %>% fct_rev()) %>% 
  ggplot() +
  geom_col(aes(region, mean), fill = "forestgreen", alpha = 0.7) + 
  geom_errorbar(aes(x = region, ymin = mean - ic,
                    ymax = mean + ic), size = 0.5, width = 0.1) +
  labs(
    title = "Standardized Maths Test Score (Wave 2)",
    x = "Region", y = "", caption = "The bars indicate 95% CI"
  )
  

# Regional Analysis:
tigray_oromia %>% 
  filter(urban ==  0) %>% 
  by_region(std_maths_w2) %>% 
  mutate(region = fct_reorder(region, mean) %>% fct_rev()) %>% 
  ggplot() +
  geom_col(aes(region, mean), fill = "forestgreen", alpha = 0.7) + 
  geom_errorbar(aes(x = region, ymin = mean - ic,
                    ymax = mean + ic), size = 0.5, width = 0.1) +
  labs(
    title = "Standardized Maths Test Score (Wave 2)",
    x = "Region", y = "", caption = "The bars indicate 95% CI"
  )
  
tigray_oromia %>% 
  filter(region == "Oromia") %>% 
  count(stdmeal)

# HIghest level of teacher education ("mth_tchlvtctr") is an important vairable to 
# consider to explain the d/ce in math test score b/n Oromia and
# Tigray

# Father's and Mother's education; School facilities (Library, electricity, etc.); 
# Number of meals per day; type of school (government, private, etc.); 
# 
oromia %>% 
  count(fclibr)

tigray %>% 
  count(fclibr)

amhara %>% 
  count(htgrdmth)

# consider the variation in english instruction at the school level
somali <- somali %>% 
  mutate(
    started_englsh = case_when(
      htgrdmth %in% c(1, 3, 5, 7) ~ "Yes",
      htgrdmth == 9 ~ "No"
    ) %>% factor()
  )

somali_plot <- somali %>% 
  group_by(started_englsh, htschown) %>% 
  summarise(
    n = n(),
    mean = mean(std_maths_w2, na.rm = TRUE),
    sd = sd(std_maths_w2, na.rm = TRUE)
  ) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(ic = se*qt(.975, n-1)) 

somali_plot %>% 
  mutate(started_englsh = fct_reorder(started_englsh, mean) %>% fct_rev()) %>% 
  ggplot() +
  geom_col(aes(started_englsh, mean), fill = "forestgreen", alpha = 0.7) + 
#  geom_errorbar(aes(x = started_englsh, ymin = mean - ic,
                    # ymax = mean + ic), size = 0.5, width = 0.1) +
  labs(
    title = "Raw Maths Test Score (Wave 2)",
    x = "",
    y = ""
  )

snnp %>% 
  count(htschown)
  
tigray %>% 
  count(stdlnghm, sort = TRUE)



oromia %>% 
  count(stdlnghm, sort = TRUE)

oromia %>% 
  filter(stdlnghm == 2) %>% 
  count(stlng05)

oromia %>% 
  count(scslngin)

oromia %>% 
  count(lang)

snnp %>% 
  count(stdlnghm, sort = TRUE) 

snnp %>% 
  count(scslngin)

amhara %>% 
  count(scslngin)

tigray %>% 
  count(scslngin)

# All sections in SNNP were taught maths in English:
snnp %>% 
  count(sect_id, scslngin) %>% 
  print(n = Inf)

# Two sections in Oromia were learning maths in Amaharic; the rest
# were taught in Afaan Oromo
oromia %>% 
  count(sect_id, scslngin) %>% 
  print(n = Inf)

# 
amhara %>% 
  count(sect_id, scslngin) %>% 
  print(n = Inf)

somali %>% 
  count(sect_id, scslngin) %>% 
  print(n = Inf)

tigray %>% 
  count(sect_id, scslngin) %>% 
  print(n = Inf)


oromia %>% 
  group_by(scslngin) %>% 
  summarize(n = n())

oromia %>% 
  filter(scslngin == 1) %>% 
  count(stdlnghm)

tigray %>% 
  count(stdlnghm, sort = TRUE)

snnp %>% 
  count(stdlnghm, sort = TRUE)

snnp %>% 
  filter(stdlnghm == 2) %>% 
  count(stlng03, name = "Guraghigna")

snnp %>% 
  filter(stdlnghm == 2) %>% 
  count(stlng04, name = "Hadiyigna")

snnp %>% 
  filter(stdlnghm == 2) %>% 
  count(stlng06, name = "Sidamigna")

snnp %>% 
  filter(stdlnghm == 2) %>% 
  count(stlng10, name = "Welaytigna")

snnp %>% 
  filter(stdlnghm == 2) %>% 
  count(stlng07, name = "Silitigna")

amhara %>% 
  count(stdlnghm, sort = T)

amhara %>% 
  pivot_longer(stlng01:stlng11, names_to = "can_speak", values_to = "value") %>% 
  mutate(
    can_speak = case_when(
      can_speak == "stlng01" ~ "Afarigna",
      can_speak == "stlng02" ~ "Amarigna",
      can_speak == "stlng03" ~ "Guraghigna",
      can_speak == "stlng04" ~ "Hadiyigna",
      can_speak == "stlng05" ~ "Af-Oromo",
      can_speak == "stlng06" ~ "Sidamigna",
      can_speak == "stlng07" ~ "Silitigna",
      can_speak == "stlng08" ~ "Af-Somali",
      can_speak == "stlng09" ~ "Tigrigna",
      can_speak == "stlng10" ~ "Welaytigna",
      can_speak == "stlng11" ~ "English",
    ) %>% factor(),
  ) %>% 
  filter(value == 1) %>% 
  count(can_speak)
  

snnp %>% 
  pivot_longer(stlng01:stlng11, names_to = "can_speak", values_to = "value") %>% 
  mutate(
    can_speak = case_when(
      can_speak == "stlng01" ~ "Afarigna",
      can_speak == "stlng02" ~ "Amarigna",
      can_speak == "stlng03" ~ "Guraghigna",
      can_speak == "stlng04" ~ "Hadiyigna",
      can_speak == "stlng05" ~ "Af-Oromo",
      can_speak == "stlng06" ~ "Sidamigna",
      can_speak == "stlng07" ~ "Silitigna",
      can_speak == "stlng08" ~ "Af-Somali",
      can_speak == "stlng09" ~ "Tigrigna",
      can_speak == "stlng10" ~ "Welaytigna",
      can_speak == "stlng11" ~ "English",
    ) %>% factor(),
  ) %>% 
  filter(value == 1) %>% 
  count(can_speak)

















