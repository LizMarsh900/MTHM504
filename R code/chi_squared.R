library(dplyr)
library(purrr)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_clean.rds")

# Load .gov data - gives the number of births (in thousands) per year and month
births <- xl.read.file(".gov_data.xlsx",
                   xl.sheet = "Sheet2")

# Create variables for total births in each quarter, total each year
# and find proportion
births <- births %>%
  mutate(
    Q1 = Sep + Oct + Nov,
    Q2 = Dec + Jan + Feb,
    Q3 = Mar + Apr + May,
    Q4 = Jun + Jul + Aug,
    total = rowSums(across(Jan:Dec)),
    across(Q1:Q4, ~ .x / total, .names = "{.col}_prop")
  )


#### Chi-squared tests of applications IN GENERAL ##############################

# Specifically this is comparing birth quarter distributions to 25% for each
# Would be more accurate to use actual census data for this

# 2021-2023 getting proportions
obs2123 <- d %>%
  filter(year_group == "2021-23") %>%
  pull(birth_quarter) %>%
  table()

# Perform chi-squared test
test2123 <- chisq.test(obs2123)

# Get residuals
test2123$residuals

# Same steps for 2022-2024
obs2224 <- d %>%
  filter(year_group == "2022-24") %>%
  pull(birth_quarter) %>%
  table()

test2224 <- chisq.test(obs2224)

test2224$residuals

#### Slightly more technical by comparing to census data #######################

# 2021-2023 cohort are born in 2004 and 2005, so average these census proportions
expected2123 <- births %>%
  filter(Year %in% c(2004, 2005)) %>%
  summarise(
    "1" = sum(Q1),
    "2" = sum(Q2),
    "3" = sum(Q3),
    "4" = sum(Q4)
  ) %>%
  unlist()

expected2123 <- expected2123 / sum(expected2123)

# Conduct chi square test
chi2123 <- chisq.test(obs2123, p = expected2123)

chi2123$observed
chi2123$expected
chi2123$residuals

# 2022-2024 cohort born in 2005 and 2006 same steps again
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

# Conduct chi square test
chi2224 <- chisq.test(obs2224, p = expected2224)

chi2224$observed
chi2224$expected
chi2224$residuals



#### Now comparing selected versus non-selected ################################

tab <- table(d$birth_quarter, d$selected)

tab

chisq.test(tab)


# Broken down by year group
d %>%
  group_by(year_group) %>%
  summarise(
    test = list(chisq.test(table(birth_quarter, selected)))
  ) %>%
  mutate(
    p_value = map_dbl(test, ~ .x$p.value),
    chisq   = map_dbl(test, ~ .x$statistic)
  )


#### Gender ####################################################################

# Create table for use in Chi-square test 
tab <- table(d$sex, d$selected)

# Test whether the sex distribution of selected applicants is same as whole pool
chisq.test(tab)

# "Prefer not to say" option caused issues in later chi-square test, 
# Since only 1 applicant has picked it, we'll remove for now
d2 <- d %>%
  filter(sex %in% c("Female", "Male")) %>%
  droplevels()

# Same test as above but broken down by year
d2 %>%
  group_by(year_group) %>%
  summarise(
    test = list(chisq.test(table(sex, selected)))
  ) %>%
  mutate(
    p_value = map_dbl(test, ~ .x$p.value),
    chisq   = map_dbl(test, ~ .x$statistic)
  )

