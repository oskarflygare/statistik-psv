
# Load packages
library(tidyverse)
library(foreign)
library(psych)

# Read data
data <- as_tibble(read.spss("spss_files_0/psychology.sav"))

# Get descriptives
descriptives <- describeBy(data, data$GROUP)

# 1st year students
descriptives[[1]]

# 2nd year students
descriptives[[2]]

# 3rd year students
descriptives[[3]]

# Check distribution of values for SOCIAL variable per year
data %>%
  ggplot(aes(x = SOCIAL)) +
  geom_bar() +
  facet_wrap(~GROUP)

# Check boxplot to spot outliers
data %>%
  ggplot(aes(x = GROUP, y = SOCIAL)) + geom_boxplot()

# Is there a significant difference between 1st year and 2nd year students in terms of SOCIAL?
data %>%
  filter(GROUP != "3rd Year") %>%
  lm(SOCIAL ~ GROUP, data = .) %>%
  summary(.)

# What is the correlation between SOCIAL and DEVELOP?

## Lets check with a plot
data %>%
  ggplot(aes(x = DEVELOP, y = SOCIAL)) + geom_point() +
  geom_smooth(method = "lm")

## Lets do a pearson's r test
corr_test <- corr.test(data$SOCIAL, data$DEVELOP)

print(corr_test, short = FALSE)
