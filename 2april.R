
# Introduction ------------------------------------------------------------

# Statistics journal club April 2nd 2019
# Repeated measures designs
# Script written by Oskar Flygare (@oskarflygare)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(psych)
library(foreign)
library(lme4)

# Read data ---------------------------------------------------------------

df_data <- as_tibble(read.spss("spss_files_0/Bushtucker.sav"))

str(df_data)


# Restructure data --------------------------------------------------------

df_long <- df_data %>%
  gather(key = "outcome", value = "value", stick:witchetty) %>%
  arrange(celebrity)

df_long <- df_long %>%
  mutate(animal = case_when(
    outcome == "stick" ~ 1,
    outcome == "testicle" ~ 2,
    outcome == "eye" ~ 3,
    outcome == "witchetty" ~ 4
  ))

# Exploratory analysis ----------------------------------------------------

ggplot(df_data, aes(x = celebrity, y = stick)) + geom_point() + geom_line()

ggplot(df_long, aes(x = outcome, y = value, group = celebrity)) + geom_point() + geom_line(colour = 1)


# Linear regression --------------------------------------------------

# First a simple linear model
(fit_lm <- lm(value ~ outcome, data = df_long))

summary(fit_lm)

