
# Introduction -------------------------------------------------------------
# Statistics journal club at Psykiatri Sydväst
# 4 December 2018 - Moderation & Mediation
# Script by Oskar Flygare

# Load packages and data -----------------------------------------------------------

library(tidyverse)
library(foreign)
library(hrbrthemes)

df_data <- as_tibble(read.spss("spss_files_0/Video Games.sav"))

str(df_data)


# Moderation --------------------------------------------------------------

# Does Vid_Game predict Aggress?
fit1 <- lm(Aggress ~ Vid_Game, data = df_data)
summary(fit1)

# Does CaUnTs predict Aggress?
fit2 <- lm(Aggress ~ CaUnTs, data = df_data)
summary(fit2)

# Is there an interaction (moderation) between Vid_Game and CaUnTs?
fit3 <- lm(Aggress ~ Vid_Game * CaUnTs, data = df_data)
summary(fit3)

# Yes!

# Add quartiles for the variables, similar to what is done in the book
df_data <- df_data %>%
  mutate(Vid_Game_quart = ntile(Vid_Game, 4),
         CaUnTs_quart = ntile(CaUnTs, 4))

# Plot the interaction between CaUnTs (as factor) and Vid_Game
df_data %>%
  ggplot(aes(x = Vid_Game, y = Aggress, colour = as.factor(CaUnTs_quart))) + geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Video games increase aggression",
       subtitle = "But only for individuals with high callous and unemotional traits") +
  scale_colour_discrete("Callous traits (quartile)") +
  theme_minimal() +
  theme(legend.position = "top")


# Mediation ---------------------------------------------------------------

# These instructions can be found at page 499-500
df_lambert <- as_tibble(read.spss("spss_files_0/Lambert et al. (2012).sav"))
str(df_lambert)

# We need three models: Using just the predictor variable, just the mediator, and then both predictor and mediator

fit_m1 <- lm(Phys_Inf ~ LnPorn, data = df_lambert)
summary(fit_m1)

fit_m2 <- lm(Commit ~ LnPorn, data = df_lambert)
summary(fit_m2)

fit_m3 <- lm(Phys_Inf ~ LnPorn + Commit, data = df_lambert)
summary(fit_m3)

# So LnPorn has an estimate of .59 in model 1, but it is only .46 in model 3. Some mediation here!

# Andy Fields suggests some other analyses that are better but I didn't have time to analyze them.
