# Load required libraries
library(excel.link)
library(tidyverse)
library(dplyr)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Check this has worked
getwd()

# Load data and assign to "d"
# Used excel.link library for this, as it works with password protected documents
# Make sure the Excel file being loaded is NOT open when you run it
# See notes on coding
d1 <- xl.read.file("data_postcode_removed.xlsx",
                  password = "Sawe15930",           #OMIT IN WRITE-UP
                  write.res.password = "Sawe15930",
                  xl.sheet = "2021-23 (CLEAN)")

d2 <- xl.read.file("data_postcode_removed.xlsx",
                  password = "Sawe15930",
                  write.res.password = "Sawe15930",
                  xl.sheet = "2022-24 (CLEAN)")


#### Wrangling ####

# There are two columns both containing LSOA code so delete one
# Necessary to do this now because `rename` doesn't work otherwise
d1$`LSOA code` <- NULL
d2$`LSOA code` <- NULL

# Join the data frames to create just one with all application years
# First change the rename variables that aren't labelled the same
d2 <- d2 %>%
  rename(
    `Event Group:` = `Select Your Event Group:`,
    `Education/Work As of Sep 21:` = `As of September 2022, in terms of education/work -  please select which applies to you?`
  )

# Can now bind together
d <- bind_rows(d1, d2)

# Rename variables to make them more useable
d <- d %>%
  rename(
    `ID` = `Unique ID`,
    `year_group` = `Year Group`,
    `selected` = `YTP Selected`,
    `centre_allocated` = `Centre Allocated`,
    `centre_choice` = `First Choice Regional Centre`,
    `decimal_age` = `Decimal Age`,
    `DOB` = `DOB (MM/YYYY)`,
    `ethnicity` = `Ethnicity:`,
    `sex` = `Sex:`,
    `disability` = `Disability:`,
    `religion` = `Religion:`,
    `education_work` = `Education/Work As of Sep 21:`,
    `qualification_pathway` = `What 'qualification pathway' have you enrolled onto as of September 2021?`,
    `years_competing` = `How many years have you been competing in athletics?`,
    `main_sport` = `Is athletics your main sport?`,
    `quit_sports` = `Have you quit other sports to focus on athletics?`,
    `months_train8` = `Do you train or participate in athletics for more than 8 months a year?`,
    `other_sports` = `What other sports do you participate in?`,
    `event_group` = `Event Group:`,
    `primary_event` = `Primary Event:`,
    `hours_week` = `How many hours per week do you typically train for your primary event?`,
    `specialist_support` = `In the past 12-months, have you had access to any specialist sport science support?`,
    `difficulty_participating` = `In the past 12-months, have you had any difficulty participating in training and/or competition due to a health problem?`,
    `LSOA_code` = `LSOA code`,
    `LSOA_name` = `LSOA Name`,
    `IMD_rank` = `Index of Multiple Deprivation Rank`,
    `IMD_decile` = `Index of Multiple Deprivation Decile`,
    `income_rank` = `Income Rank`,
    `income_decile` = `Income Decile`,
    `income_score` = `Income Score`,
    `employment_rank` = `Employment Rank`,
    `employment_decile` = `Employment Decile`,
    `employment_score` = `Employment Score`,
    `education_rank` = `Education and Skills Rank`,
    `education_decile` = `Education and Skills Decile`,
    `health_rank` = `Health and Disability Rank`,
    `health_decile` = `Health and Disability Decile`,
    `crime_rank` = `Crime Rank`,
    `crime_decile` = `Crime Decile`,
    `housing_rank` = `Barriers to Housing and Services Rank`,
    `housing_decile` = `Barriers to Housing and Services Decile`,
    `environment_rank` = `Living Environment Rank`,
    `environment_decile` = `Living Environment Decile`,
    `IDACI_rank` = `IDACI Rank`,
    `IDACI_decile` = `IDACI Decile`,
    `IDACI_score` = `IDACI Score`,
    `IDAOPI_rank` = `IDAOPI Rank`,
    `IDAOPI_decile` = `IDAOPI Decile`,
    `IDAOPI_score` = `IDAOPI Score`,
  )

# Delete rows with duplicated applications that have no data
# and the participants that were accepted without application
# i.e. all the rows that have anything in the comment column (31 participants)
d <- d %>% 
  filter(is.na(Comment))

# Create birth date quartiles 
# Q1 - September to November (09-11)
# Q2 - December to February (12-02)
# Q3 - March to May (03-05)
# Q4 - June to August (06-08)
d <- d %>%
  mutate(
    birth_quarter = case_when(
      as.numeric(substr(DOB, 1, 2)) %in% c(9, 10, 11) ~ "1",
      as.numeric(substr(DOB, 1, 2)) %in% c(12, 1, 2)  ~ "2",
      as.numeric(substr(DOB, 1, 2)) %in% c(3, 4, 5)   ~ "3",
      as.numeric(substr(DOB, 1, 2)) %in% c(6, 7, 8)   ~ "4"
    )
  )
