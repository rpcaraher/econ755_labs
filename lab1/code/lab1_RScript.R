################################################################################
## Lab 1
## Econ 755
################################################################################

## First, we have to load our R packages.
## Don't forget to install them if you haven't yet!

library(tidyverse)

## Now, lets clear our environment

rm(list = ls())

## Let's read in the ACS data

acs <- read_csv("lab1/data/acs_sample.csv")

## Don't forget to take a look at the Readme file!

## We should probably filter our data to exclude those not working.
## Let's do this using filter and overwrite our existing object

acs_wrk <- acs %>%
  filter(labforce == 2)

## Not let's create a visualization.
## Let's look at the distribution of wages.

ggplot(acs_wrk) +
  geom_histogram(aes(x = incwage))

## That's clearly skewed. Let's make a new variable that is the log of wage.

acs_wrk <- acs_wrk %>%
  mutate(log_wage = log(incwage))

## Let's try that again.

ggplot(acs_wrk) +
  geom_histogram(aes(x = log_wage))

## Let's look at average salary by gender. 
## Summarize allows us to do computations by group,
## as long as the line proceeding it is a group_by() command

acs_wrk %>%
  group_by(sex) %>%
  summarise(avg_sal = mean(incwage, na.rm = T))

## Let's do it again, but this time by race and gender.

acs_wrk %>%
  group_by(sex, race) %>%
  summarise(avg_sal = mean(incwage, na.rm = T))

## OK, race is coded as a bunch of numbers.
## Let's use the readme file to recode these using the case_when function.

acs_wrk <- acs_wrk %>%
  mutate(race_rec = case_when(race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "native",
    race %in% c(4, 5, 6) ~ "asian_pac",
    race == 7 ~ "other",
    race %in% c(8, 9) ~ "mixed",
    is.na(race) ~ NA_character_
   ))

# Let's look at the data again to make sure it worked

count(acs_wrk, race, race_rec)

## Now let's try that again with our new variable.

acs_wrk %>%
  group_by(sex, race_rec) %>%
  summarise(avg_sal = mean(incwage, na.rm = T))

## Let's just focus on the white and black populations.

acs_wrk %>%
  filter(race_rec %in% c("white", "black")) %>%
  group_by(sex, race_rec) %>%
  summarise(avg_sal = mean(incwage, na.rm = T))

## Let's create another visualization.
## Let's look at a scatterplot between age and wage income.

ggplot(acs_wrk) +
  geom_point(aes(x = age, y = incwage))

## Now let's run a regression of wage income on sex to estimate the wage gap.
## Lets first create an indicator for female

acs_wrk <- acs_wrk %>%
  mutate(female = case_when(sex == "female" ~ 1,
    sex == "male" ~ 0,
    TRUE ~ NA_real_))

## First, lets drop all invalid wage observations

acs_wrk <- acs_wrk %>%
  filter(!is.infinite(log_wage))


lm_wage <- lm(log_wage ~ female, data = acs_wrk)

## Let's look at the results.

summary(lm_wage)

## Let's try that again, but this time lets control for age

lm_wage_age <- lm(log_wage ~ age + female, data = acs_wrk)

## Let's look at the results.

summary(lm_wage_age)

## Is there an additiona wage gap for black women?
## Let's use an interaction term and re-run the regression.

acs_wrk <- acs_wrk %>%
  mutate(black = case_when(race_rec == "black" ~ 1,
    is.na(race_rec) ~ NA_real_,
    TRUE ~ 0))

lm_wage_race <- lm(log_wage ~ age + female + black + female * black, data = acs_wrk)
summary(lm_wage_race)

## Now let's run a panel regression, using states as fixed effects.
## There are lots of great packages with nice implemntation of fixed effects models.
## But let's use my favorite: fixest

## Let's install it first, if you haven't already.
## Do so by running the following line of code that is commented out:
## install.packages("fixest")

library(fixest)

## Now let's the same basic linear and make sure it gives the same results.
## The syntax for feols is a little different, 
## so check the docuentation if you're confused using ?fixest::feols

lm_wage <- feols(log_wage ~ female | 0,
  se = "iid",
data = acs_wrk)

lm_wage_race <- feols(log_wage ~ age + female + black + female * black | 0,
se = "iid",
data = acs_wrk)

summary(lm_wage_race)

## Now let's add state fixed effects and clustered standard errors

lm_wage_race_sfe <- feols(log_wage ~ age + female + black + female * black | state,
cluster = "state",
data = acs_wrk)

summary(lm_wage_race_sfe)

## Great, we have our results. Let's save them and export them as .tex files.

## First, let's save the summary table.
## There are lots of ways to do this in R, but the simplest is stargazer,
## which is a great package for exporting regression tables as well.
## Let's install it first, if you haven't already.
## Do so by running the following line of code that is commented out:
## install.packages("stargazer")

library(stargazer)

## Now let's save the summary table.

desc_tab <- acs_wrk %>%
  filter(race_rec %in% c("white", "black")) %>%
  group_by(sex, race_rec) %>%
  summarise(avg_sal = mean(incwage, na.rm = T)) %>%
  ungroup()

desc_tab

## These labels don't look great. Let's change them.

desc_tab <- desc_tab %>%
  rename(Gender = sex,
  Race = race_rec,
  `Avg. Salary` = avg_sal)

desc_tab

## Let's also correct the capitalization

desc_tab <- desc_tab %>%
  mutate(Race = case_when(Race == "black" ~ "Black",
    TRUE ~ Race))

desc_tab

## Let's also round the numbers to whole numbers

desc_tab <- desc_tab %>%
  mutate(`Avg. Salary` = round(`Avg. Salary`, 0))

## Great. Now let's run the command to actually export it.
## Let's export it to the "results/" folder as a .tex file.

stargazer(desc_tab, summary = FALSE, 
  type = "latex",
  float = FALSE,
  out = file.path("lab1", "results", "desc_tab.tex"))

## Let's also export our figure which shows the distribution of wages.

p1 <- ggplot(acs_wrk) +
  geom_histogram(aes(x = incwage))

print(p1)

## This will be impossible to read. Let's fix it.

p1 <- ggplot(acs_wrk) +
  geom_histogram(aes(x = incwage)) +
  labs(x = "Log Wage", y = "Frequency") +
  theme(axis.title = element_text(size = 20),
    axis.text = element_text(size = 15))

print(p1)

## Now let's export it.
## ggsave() will export the last plot you created.

ggsave(file.path("lab1", "results", "hist_wage.pdf"), 
  width = 10, height = 10, units = "in")

## Now let's export our regression results.
## The fixest package has a built in function to export regression results.
## Let's use it.

## The setFixest_dict function allows us to set the 
## names of the variables in the regression output.
## Let's use it to make our table look nice.


setFixest_dict(c(log_wage = "Log wage", 
    age = "Age",
    female = "Female",
    black = "Black",
    `female:black` = "Female x Black",
    state = "State"))

## Now let's export the regression results using the etable function.
## There are lots of options, so look at the documentation wuth ?etable.

etable(list(
    lm_wage,
    lm_wage_race,
    lm_wage_race_sfe
    ),
    fitstat = c("n", "r2"),
    digits = "r4",
    digits.stats = "r4",
    tex = T,
    replace = T,
    meta.time = T,
    meta.author = T,
    file = file.path("lab1", "results", "reg_tab.tex"))

################################################################################
## I like to print a message to the console when the script is done running.

mssg <- paste0("Script complete at ", Sys.time())

print(mssg)

################################################################################