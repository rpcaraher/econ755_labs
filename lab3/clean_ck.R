## Load packages

library(haven)
library(stargazer)
library(fixest)
library(tidyverse)
## Set options

options(scipen = 999)

## Clear environment

rm(list = ls())

## Set directories

base_directory <- '/Users/rcaraher/Library/CloudStorage/OneDrive-UniversityofMassachusetts/Academic/Teaching/ECON 755/Problem Sets'
data_directory <- file.path(base_directory, 'Data')
results_directory <- file.path(base_directory, 'Results')

## Download data

tempfile_path <- tempfile()
download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", destfile = tempfile_path)
tempdir_path <- tempdir()
unzip(tempfile_path, exdir = tempdir_path)
codebook <- read_lines(file = paste0(tempdir_path, "/codebook"))

variable_names <- codebook %>%
  `[`(8:59) %>%
  `[`(-c(5, 6, 13, 14, 32, 33)) %>%
  str_sub(1, 13) %>%
  str_squish() %>%
  str_to_lower()

dataset <- read_table2(paste0(tempdir_path, "/public.dat"),
                       col_names = FALSE)

dataset <- dataset %>%
  select(-X47) %>%
  `colnames<-`(., variable_names) %>%
  mutate_all(as.numeric) %>%
  mutate(sheet = as.character(sheet))

ck <- dataset

## Fix variables and layout of file

## Select specific variables

ck <- ck %>%
  select(sheet, chain, state, southj, centralj, shore, pa1, pa2, 
         empft, emppt, nmgrs, wage_st, hrsopen, psoda, pfry, pentree, 
         empft2, emppt2, nmgrs2, wage_st2, hrsopen2, psoda2, pfry2, pentree2)

ck <- ck %>%
  rename(empft_bef = empft,
         emppt_bef = emppt,
         nmgrs_bef = nmgrs,
         stwage_bef = wage_st,
         hrsopen_bef = hrsopen,
         psoda_bef = hrsopen,
         pfry_bef = hrsopen,
         pentree_bef = hrsopen,
         empft_aft = empft2,
         emppt_aft = emppt2,
         nmgrs_aft = nmgrs2,
         stwage_aft = wage_st2,
         hrsopen_aft = hrsopen2,
         psoda_aft = hrsopen2,
         pfry_aft = hrsopen2,
         pentree_aft = hrsopen2
         )

## Pivot data

ck_l <- ck %>%
  pivot_longer(
    cols = starts_with("empft") | starts_with("emppt") | starts_with("nmgrs") | 
      starts_with("stwage") | starts_with("hrsopen") | starts_with("psoda") | 
      starts_with("pfry") | starts_with("pentree"),
    names_to = c(".value", "period"),
    names_pattern = "(.*)_(bef|aft)"
  ) %>%
  filter(!is.na(period))

## Compute FTE

ck_l <- ck_l %>% 
  mutate(fte=empft+nmgrs+(0.5*emppt))

## Recode State and period

ck_l <- ck_l %>%
  mutate(state = case_when(state == 0 ~ "PA",
                           state == 1 ~ "NJ"),
         period = case_when(period == "bef" ~ "pre",
                            period == "aft" ~ "post"),
         time = case_when(period == "pre" ~ 1,
                          period == "post" ~ 2))

ck_l %>%
  group_by(state, time) %>%
  summarise(m_fte = mean(fte, na.rm = T))


## Recode other variables

ck_l <- ck_l %>%
  mutate(chain = case_when(chain == 1 ~ "BK",
                           chain == 2  ~ "KFC",
                           chain == 3 ~ "Roys",
                           chain == 4 ~ "Wendys"
                           ))


ck_l <- ck_l %>%
  rename(rest_id = sheet)

ck_l <- ck_l %>%
  mutate(post = case_when(period == "post" ~ 1,
                          period == "pre" ~ 0),
         treated = case_when(state == "NJ" ~ 1,
                             state == "PA" ~ 0))


lm(fte ~ treated + post + post * treated, data = ck_l)

write_csv(ck_l, "ck_1994.csv")

