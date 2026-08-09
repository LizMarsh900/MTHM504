library(dplyr)
library(purrr)
library(excel.link)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_revised_clean.rds")

# Load .gov data - gives the number of births (in thousands) per year and month
# See table 2.3 at https://assets.publishing.service.gov.uk/media/5a7c89aae5274a0bb7cb7b1b/9787777148210.pdf
births <- xl.read.file(".gov_data.xlsx",
                   xl.sheet = "Sheet2")

# Create variables for total births in each quarter, total each year
# and find proportions
births <- births %>%
  mutate(
    Q1 = Sep + Oct + Nov,
    Q2 = Dec + Jan + Feb,
    Q3 = Mar + Apr + May,
    Q4 = Jun + Jul + Aug,
    total = rowSums(across(Jan:Dec)),
    across(Q1:Q4, ~ .x / total, .names = "{.col}_prop")
  )


#### Chi-square comparing to census data #######################################

# Making table of 2021-23 birth quarters and numbers in each
obs2123 <- d %>%
  filter(year_group == "2021-23") %>%
  pull(birth_quarter) %>%
  table()

# 2021-2023 cohort are born in 2004 and 2005, so average these census proportions
# Create a table of averaged total live births
expected2123 <- births %>%
  filter(Year %in% c(2004, 2005)) %>%
  summarise(
    "1" = sum(Q1),
    "2" = sum(Q2),
    "3" = sum(Q3),
    "4" = sum(Q4)
  ) %>%
  unlist()

# Find proportions (instead of just counts)
expected2123 <- expected2123 / sum(expected2123)

# Conduct chi square test
chi2123 <- chisq.test(obs2123, p = expected2123)

chi2123$observed
chi2123$expected
chi2123$residuals


#### Same steps again for 2022-2024 ####
obs2224 <- d %>%
  filter(year_group == "2022-24") %>%
  pull(birth_quarter) %>%
  table()

# 2022-2024 cohort born in 2005 and 2006
expected2224 <- births %>%
  filter(Year %in% c(2005, 2006)) %>%
  summarise(
    "1" = sum(Q1),
    "2" = sum(Q2),
    "3" = sum(Q3),
    "4" = sum(Q4)
  ) %>%
  unlist()

expected2224 <- expected2224 / sum(expected2224)

chi2224 <- chisq.test(obs2224, p = expected2224)

chi2224$observed
chi2224$expected
chi2224$residuals

