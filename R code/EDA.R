# Load required libraries
library(broom) #for summarising information into tidy tibbles HASNT BEEN USED YET
library(gtsummary) #for creating tables
library(ggplot2) #for visualisation

# Load clean data (if not already loaded)
d <- readRDS("data_clean.rds")


#### Exploratory Data Analysis #################################################

table(d$year_group) 
# 310 applicants in 2021-23 and 381 applicants in 2022-24

table(d$year_group, d$selected)
# 240 selected in 2021 (70 not), and 242 in 2022 (138 not)


#### Characteristics/demographics tables ####

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

# Event table
d %>%
  select(year_group, event_group, primary_event) %>%
  tbl_summary(
    by = year_group,
    label = list(
      event_group ~ "Event group",
      primary_event ~ "Primary event"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  )


#### More detailed exploration of decimal age ####

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

# Want to find out how many applicants were selected AND older than 17
queries <- filter(
  d, selected == "Yes" & decimal_age >= 17)

# Any that are younger?
queries2 <- filter(
  d, selected == "Yes" & decimal_age <= 15)
#NONE


#### Exploration of deprivation variables ####

# Mostly interested in IMD and IDACI-not particularly interested in sub-scales
summary(d$IMD_rank)

# Visualisation of distribution of IMD decile
d %>%
  filter(year_group == "2021-23") %>%
ggplot(aes(x = factor(IMD_decile))) +
  geom_bar(fill = "lightgrey", colour = "black") +
  labs(
    x = "IMD Decile",
    y = "Count",
    title = "Distribution of IMD Deciles (2021-23)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

d %>%
  filter(year_group == "2022-24") %>%
  ggplot(aes(x = factor(IMD_decile))) +
  geom_bar(fill = "lightgrey", colour = "black") +
  labs(
    x = "IMD Decile",
    y = "Count",
    title = "Distribution of IMD Deciles (2022-24)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Visualisation of distribution of IDACI deciles
d %>%
  filter(year_group == "2021-23") %>%
  ggplot(aes(x = factor(IDACI_decile))) +
  geom_bar(fill = "lightgrey", colour = "black") +
  labs(
    x = "IDACI Decile",
    y = "Count",
    title = "Distribution of IDACI Deciles (2021-23)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

d %>%
  filter(year_group == "2022-24") %>%
  ggplot(aes(x = factor(IDACI_decile))) +
  geom_bar(fill = "lightgrey", colour = "black") +
  labs(
    x = "IDACI Decile",
    y = "Count",
    title = "Distribution of IDACI Deciles (2022-24)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
