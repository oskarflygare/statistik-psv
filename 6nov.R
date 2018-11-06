
# Introduction ------------------------------------------------------------

# Script related to the statistics journal club at PSV
# Today's topic is correlation & linear regression
# Chapters 8 and 9 in the Andy Field book

library(tidyverse)
library(foreign)
library(psych)
library(broom)


# Read data ---------------------------------------------------------------

data <- as_tibble(read.spss("spss_files_0/Album Sales.sav"))



# Check data structure ----------------------------------------------------

str(data)

head(data)

hist(data$Adverts)

hist(data$Sales)

hist(data$Airplay)

hist(data$Image)

# Correlation -------------------------------------------------------------

data %>%
  ggplot(aes(x = Adverts, y = Sales)) + geom_point() +
  geom_smooth(method = "lm")

cor.test(data$Adverts, data$Sales, method = "spearm")

# Regression --------------------------------------------------------------

# Lets fit a single predictor first
fit1 <- lm(Sales ~ Adverts, data = data)

summary(fit1)

# Ah, looks like we have some heteroscedasticity
ggplot(lm(Sales ~ Adverts, data = data)) +
  geom_point(aes(x = .fitted, y = .resid))

plot(fit1)

# Let's do a Weighted Least Squares regression instead
fit2 <- glm(Sales ~ Adverts, data = data)

summary(fit2)

# Okay, now lets do a multiple regression analysis

fit3 <- lm(Sales ~ Adverts + Airplay + Image, data = data)

summary(fit3)
