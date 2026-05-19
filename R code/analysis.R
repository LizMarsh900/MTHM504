#### Set-up ####################################################################

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


#### Wrangling #################################################################

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


#### Birth date quartiles ####

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


#### Education/work variable ####

# Wrangling work_education (as of September) variable
# Drop down menu of options: State school/Independent school/Apprenticeship/
# Working/Other, where "Other" they wrote a response
table(d$education_work)
# Mostly Colleges, a couple of grammar schools, and miscellaneous other stuff

# Create list for the for loop later, can be edited to incorporate more typos
education_work_list <- c(
  `State School` = "state school",
  `Independent School` = "independent school",
  `Apprenticeship` = "apprenticeship",
  `Working` = "working",
  `Grammar School` = "grammar",
  `College` = "college",
  `Unknown` = "to be confirmed|unknown",
  `Academy` = "academy"
)

# Create default column
d$education_work_clean <- "Other"

# For loop to create a new variable with cleaned education pathways
for (e in names(education_work_list)) {
  pattern <- education_work_list[[e]]
  d$education_work_clean[
    d$education_work_clean == "Other" & #only rows labelled other can be updated
      grepl(pattern, d$education_work, ignore.case = TRUE)
  ] <- e
}

# Sanity check
table(d$education_work_clean)
# "Other" comprises: church school, BWFC scholarship with education, Free School,
# Home schooling.


#### Religion #####

#Now checking religion
table(d$religion)
# Some messiness in terms of people specifying catholicism

# Create a religion list
religion_list <- c(
  `Christian` = "catholic|christian"
)

# Create default column
d$religion_clean <- d$religion

# For loop to create a new variable with cleaned education pathways
for (r in names(religion_list)) {
  pattern <- religion_list[[r]]
  d$religion_clean[
      grepl(pattern, d$religion, ignore.case = TRUE)
  ] <- r
}


#### Qualification pathway ####

# Now checking qualification pathway, options were: BTECs/A Levels/IB/NVQs/
# Apprenticeship/Other
table(d$qualification_pathway)
# lots of GCSEs, some combination qualifications, CTECs, T-levels, undergraduate

# Create list for the for loop later, can be edited to incorporate more typos
qualification_list <- c(
  `GCSEs and below` = "GCSE|GSCE|Year 11|Year 9",
  `Vocational Qualification` = "CTEC|cambridge technical|t[ -]?levels?|dip(loma)?|level [23]|vocational",
  `Undergraduate` = "undergraduate",
  `Unknown` = "unknown",
  `Combined A-Levels and BTECs` = "a[ -]?levels?.*btec|btec.*a[ -]?levels?",
  `BTECs` = "btec",
  `A-Levels` = "a[ -]?levels?",
  `IB` = "International Baccalaureate",
  `Apprenticeship` = "apprenticeship"
)

# Create default column
d$qualification_pathway_clean <- "Other"

# For loop to create a new variable with cleaned education pathways
for (q in names(qualification_list)) {
  pattern <- qualification_list[[q]]
  d$qualification_pathway_clean[
    d$qualification_pathway_clean == "Other" & #only rows labelled other can be updated
      grepl(pattern, d$qualification_pathway, ignore.case = TRUE)
  ] <- q
}

table(d$qualification_pathway_clean)


#### Level of specialisation variable ####

# Level of specialisation variable, based on three y/n questions
# <=1 low
# 2 moderate
# 3 high
d <- d %>%
  mutate(
    yes_count = (main_sport == "Yes") + 
      (quit_sports == "Yes") + 
      (months_train8 == "Yes"),
    specialisation = case_when(
      yes_count <= 1 ~ "low",
      yes_count == 2 ~ "moderate",
      yes_count == 3 ~ "high"
    )
  )


#### Other sports binary variables ####

# Create binary variables for each extra sport applicants do
table(d$other_sports)

sports_list <- c(football = "football",
                 tennis = "tennis",
                 swimming = "swim",
                 rugby = "rugby",
                 athletics = "athletics",
                 basketball = "basketball",
                 gymnastics = "gymnastics|trampolining",
                 badminton = "badminton",
                 golf = "golf",
                 squash = "squash",
                 cricket = "cricket",
                 hockey = "hockey",
                 cross_country = "cross country",
                 climbing = "climbing",
                 shooting = "shooting",
                 cycling_biking = "cycl|biking|mtb|bmx",
                 dance = "dance",
                 netball = "netball",
                 volleyball = "volleyball",
                 motorsports = "karting",
                 skiing = "skiing",
                 rounders = "rounders",
                 horse_riding = "horse riding|show jumping",
                 lacrosse = "lacrosse",
                 handball = "handball",
                 powerlifting = "powerlifting",
                 martial_arts = "martial arts|karate|kick boxing|pencak silat|Taekwondo|kung fu|judo",
                 water_sports = "surfing|paddling boarding|sailing|rowing|aquathlon",
                 fencing = "fencing",
                 triathlon = "triathlon",
                 fitness_classes = "fitness classes|yoga|pilates")

for (s in names(sports_list)) {
  pattern <- sports_list[[s]]
  d[[s]] <- ifelse(
    grepl(pattern, d$other_sports, ignore.case = TRUE),
    1,
    0
  )
}
