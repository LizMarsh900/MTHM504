# Load required libraries
library(broom) #for summarising information into tidy tibbles HASNT BEEN USED YET
library(gtsummary) #for creating tables
library(ggplot2) #for visualisation
library(mice) #for multiple imputation


#### Exploratory Data Analysis #################################################

table(d$year_group) 
# 310 applicants in 2021-23 and 381 applicants in 2022-24

table(d$year_group, d$selected)
# 240 selected in 2021 (70 not), and 242 in 2022 (138 not)

# Demographics table broken down by year group
d %>%
  select(year_group, sex, decimal_age, disability, ethnicity_clean,
         religion_clean) %>%
  tbl_summary(
    by = year_group,
    label = list(
      decimal_age ~ "Age (years)",
      sex ~ "Sex",
      ethnicity_clean ~ "Ethnicity",
      disability ~ "Disability",
      religion_clean ~ "Religion"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  )

# Education table
d %>%
  select(year_group, education_work_clean, qualification_pathway_clean) %>%
  tbl_summary(
    by = year_group,
    label = list(
      education_work_clean ~ "Education set-up",
      qualification_pathway_clean ~ "Qualification pathway"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  )

# Birth quarter table
d %>%
  select(year_group, birth_quarter) %>%
  tbl_summary(
    by = year_group,
    label = list(
      birth_quarter ~ "Birth quarter"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  )

# Sports/performance related demographics table
d %>%
  select(year_group, years_competing, specialisation, hours_week,
         specialist_support, health_problem) %>%
  tbl_summary(
    by = year_group,
    label = list(
      years_competing ~ "Years competing",
      specialisation ~ "Level of specialisation",
      hours_week ~ "Training hours per week",
      specialist_support ~ "Access to specialist support in previous year",
      health_problem ~ "Health problem in previous year"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  )

# Further exploration of age
summary(d$decimal_age)

# Broken down by selection and year group
d %>%
  group_by(selected, year_group) %>%
  summarise(
    n = n(),
    mean_age = mean(decimal_age, na.rm = TRUE),
    median_age = median(decimal_age, na.rm = TRUE),
    sd_age = sd(decimal_age, na.rm = TRUE),
    min_age = min(decimal_age, na.rm = TRUE),
    max_age = max(decimal_age, na.rm = TRUE)
  )

# Visualisation of age distribution among selected (separate plots for years)
d %>%
  filter(selected == "Yes", year_group == "2021-23") %>%
  ggplot(aes(x = decimal_age)) +
  geom_histogram(bins = 20,
                 fill = "lightgrey",
                 colour = "black",
                 linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "Age Distribution Among Selected Applicants (2021-23)",
    x = "Age",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

d %>%
  filter(selected == "Yes", year_group == "2022-24") %>%
  ggplot(aes(x = decimal_age)) +
  geom_histogram(bins = 20,
                 fill = "lightgrey",
                 colour = "black",
                 linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "Age Distribution Among Selected Applicants (2022-24)",
    x = "Age",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Visualisation of age distribution all applicants (separate plots for years)
d %>%
  filter(year_group == "2021-23") %>%
  ggplot(aes(x = decimal_age)) +
  geom_histogram(bins = 30,
                 fill = "lightgrey",
                 colour = "black",
                 linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "Age Distribution Among All Applicants (2021-23)",
    x = "Age",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Visualisation of age distribution all applicants (separate plots for years)
d %>%
  filter(year_group == "2022-24") %>%
  ggplot(aes(x = decimal_age)) +
  geom_histogram(bins = 30,
                 fill = "lightgrey",
                 colour = "black",
                 linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "Age Distribution Among All Applicants (2022-24)",
    x = "Age",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )





#### Multiple Imputation #######################################################

# first 54 applicants from 2022-24 cohort have missing data for these Qs
# Use multiple imputation for these

# First check missingness
sum(is.na(d$main_sport)) #52
sum(is.na(d$quit_sports)) #52
sum(is.na(d$months_train8)) #0 - so actually no missing data here

method <- make.method(d)
method[] <- ""

method["main_sport"] <- "logreg"
method["quit_sports"] <- "logreg"

# impute missing data for 3 Qs using mice (best for y/n outcomes-log regression)
imp <- mice(d,
            m = 5,
            method = method,
            seed = 123)

# Extract imputed datasets
imp_d <- complete(imp, action = "all")

# Create specialisation variable in each of the imputed datasets
imp_d <- lapply(imp_d, function(d) {
  d$specialisation <- with(d,
                           ifelse(main_sport == "Yes" &
                                    quit_sports == "Yes" &
                                    months_train8 == "Yes",
                                  "High",
                                  ifelse(main_sport == "Yes",
                                         "Moderate",
                                         "Low"))
  )
  d$specialisation <- factor(d$specialisation,
                             levels = c("Low", "Moderate", "High"))
  d
})