#### Set-up ####################################################################

# Load required libraries
library(excel.link) #for loading excel data with password protection
library(tidyverse) 
library(dplyr)


# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

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
    `ID` = "Unique ID",
    `year_group` = "Year Group",
    `selected` = "YTP Selected",
    `centre_allocated` = "Centre Allocated",
    `centre_choice` = "First Choice Regional Centre",
    `decimal_age` = "Decimal Age",
    `DOB` = "DOB (MM/YYYY)",
    `ethnicity` = "Ethnicity:",
    `sex` = "Sex:",
    `disability` = "Disability:",
    `religion` = "Religion:",
    `education_work` = "Education/Work As of Sep 21:",
    `qualification_pathway` = "What 'qualification pathway' have you enrolled onto as of September 2021?",
    `years_competing` = "How many years have you been competing in athletics?",
    `main_sport` = "Is athletics your main sport?",
    `quit_sports` = "Have you quit other sports to focus on athletics?",
    `months_train8` = "Do you train or participate in athletics for more than 8 months a year?",
    `other_sports` = "What other sports do you participate in?",
    `event_group` = "Event Group:",
    `primary_event` = "Primary Event:",
    `hours_week` = "How many hours per week do you typically train for your primary event?",
    `specialist_support` = "In the past 12-months, have you had access to any specialist sport science support?",
    `health_problem` = "In the past 12-months, have you had any difficulty participating in training and/or competition due to a health problem?",
    `LSOA_code` = "LSOA code",
    `LSOA_name` = "LSOA Name",
    `IMD_rank` = "Index of Multiple Deprivation Rank",
    `IMD_decile` = "Index of Multiple Deprivation Decile",
    `income_rank` = "Income Rank",
    `income_decile` = "Income Decile",
    `income_score` = "Income Score",
    `employment_rank` = "Employment Rank",
    `employment_decile` = "Employment Decile",
    `employment_score` = "Employment Score",
    `education_rank` = "Education and Skills Rank",
    `education_decile` = "Education and Skills Decile",
    `health_rank` = "Health and Disability Rank",
    `health_decile` = "Health and Disability Decile",
    `crime_rank` = "Crime Rank",
    `crime_decile` = "Crime Decile",
    `housing_rank` = "Barriers to Housing and Services Rank",
    `housing_decile` = "Barriers to Housing and Services Decile",
    `environment_rank` = "Living Environment Rank",
    `environment_decile` = "Living Environment Decile",
    `IDACI_rank` = "IDACI Rank",
    `IDACI_decile` = "IDACI Decile",
    `IDACI_score` = "IDACI Score",
    `IDAOPI_rank` = "IDAOPI Rank",
    `IDAOPI_decile` = "IDAOPI Decile",
    `IDAOPI_score` = "IDAOPI Score",
  )

# Delete rows with duplicated applications that have no data (27 applicants)
# and the applicants that were accepted without application (3 applicants)
# and the applicants that had data deleted for "other" reason (1 applicant)
# i.e. all the rows that have anything in the comment column (31 total)
d <- d %>% 
  filter(is.na(Comment))

# Convert "Question omitted" into NAs
vars <- names(d)

d[vars] <- lapply(d[vars], function(x) {
  x[x %in% c("Question omitted", "")] <- NA
  x
})

# The NAs in disability column correspond to no disability so change coding
d <- d %>%
  mutate(disability = replace_na(disability, "No"))

# Check structure of dataframe
str(d)

# Lots of variables need to be changed from character to factor
# Leaving out Id-type variables, and variables to be altered later
# Doing it manually so that the levels are ordered in a more meaningful way
# mostly making sure numerical things are ordered correctly,
# otherwise alphabetical and yes-no-unknown ordering
levels_list <- list(
  year_group = c("2021-23", "2022-24"),
  selected = c("Yes", "No"),
  centre_choice = c("Bath", "Birmingham", "Leeds", "London - East",
                    "London - West", "Loughborough", "Manchester"),
  sex = c("Female", "Male", "Prefer not to say"),
  disability = c("Yes", "No"),
  years_competing = c("1-2 years", "3-4 years", "5-6 years", "7-8 years",
                      "9-10 years", "More than 10 years"),
  main_sport = c("Yes", "No"),
  quit_sports = c("Yes", "No"),
  months_train8 = c("Yes", "No"),
  event_group = c("Combined Events", "Endurance", "Jumps", "Sprints and Hurdles",
                  "Throws"),
  primary_event = c("100m", "200m", "300/400m", "800m", "1500m", "3000m",
                    "5000m", "10000m", "100h", "110h", "400H", "Combined Events", 
                    "Discus", "Hammer", "High Jump", "Javelin", "Long Jump",
                    "Pole Vault", "Race Walking" , "Shot", "Steeplechase",
                    "Triple Jump"),
  hours_week = c("1-2 hours", "3-4 hours", "5-6 hours", "7-8 hours", 
                 "9-10 hours", "11-12 hours", "More than 12 hours"),
  specialist_support = c("Yes", "No"),
  health_problem = c("Yes", "No", "Don't know")
)

for (l in names(levels_list)) {
  d[[l]] <- factor(d[[l]], levels = levels_list[[l]])
}


#### Birth date quartiles ####

# Create birth date quartiles 
# Q1 - September to November (09-11)
# Q2 - December to February (12-02)
# Q3 - March to May (03-05)
# Q4 - June to August (06-08)
d <- d %>%
  mutate(
    birth_month = as.numeric(substr(DOB, 1, 2)),
                             
    birth_quarter = case_when(
      birth_month %in% c(9, 10, 11) ~ "1",
      birth_month %in% c(12, 1, 2) ~ "2",
      birth_month %in% c(3, 4, 5) ~ "3",
      birth_month %in% c(6, 7, 8) ~ "4"
    ),
    birth_quarter = as.factor(birth_quarter)
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
  `Unknown` = "to be confirmed|unknown"
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

# Convert to factor
d$education_work_clean <- factor(d$education_work_clean, levels = c(
  "Apprenticeship", "College", "Grammar School", "Independent School",
  "State School", "Working", "Unknown", "Other"
))

# Sanity check
table(d$education_work_clean)
# "Other" comprises: church school, BWFC scholarship with education, Free School,
# Home schooling, academies.


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

# Happy for this to just be alphabetical, not going to specify the level order
d$religion_clean <- as.factor(d$religion_clean)

table(d$religion_clean)


#### Ethnicity ####

# Quick check, options were: White / Asian or Asian British /
# Black or Black British / Mixed / Prefer not to say
table(d$ethnicity)
# One person specified "White South African" which should come under "White"

# Create a ethnicity list
ethnicity_list <- c(
  `White (English/Welsh/Scottish/Northern Irish/British/Other)` = 
    "White South African"
)

# Create default column
d$ethnicity_clean <- d$ethnicity

# For loop to create a new variable with cleaned education pathways
for (e in names(ethnicity_list)) {
  pattern <- ethnicity_list[[e]]
  d$ethnicity_clean[
    grepl(pattern, d$ethnicity, ignore.case = TRUE)
  ] <- e
}

d$ethnicity_clean <- factor(d$ethnicity_clean, levels = c(
  "Asian or Asian British (Bangladeshi/Indian/Pakistani/Chinese/Other)",
  "Black or Black British (African/Caribbean/Other)",
  "Mixed (White & Black Caribbean/White & Black African/White & Asian/Other)",
  "White (English/Welsh/Scottish/Northern Irish/British/Other)",
  "Prefer not to say"
))

table(d$ethnicity_clean)


#### Qualification pathway ####

# Now checking qualification pathway, options were: BTECs/A Levels/IB/NVQs/
# Apprenticeship/Other
table(d$qualification_pathway)
# lots of GCSEs, some combination qualifications, CTECs, T-levels, undergraduate

# Create list for the for loop later, can be edited to incorporate more typos
qualification_list <- c(
  `GCSEs and below` = "GCSE|GSCE|Year 11|Year 9",
  `Vocational Qualifications` = 
    "CTEC|cambridge technical|t[ -]?levels?|dip(loma)?|level [23]|vocational",
  `Undergraduate` = "undergraduate",
  `Unknown` = "unknown",
  `Combined A-Levels and BTECs` = "a[ -]?levels?.*btec|btec.*a[ -]?levels?",
  `BTECs` = "btec",
  `A-Levels` = "a[ -]?levels?",
  `International Baccalaureate` = "International Baccalaureate",
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

d$qualification_pathway_clean <- 
  factor(d$qualification_pathway_clean, levels = c(
  "A-Levels", "Apprenticeship", "BTECs", "Combined A-Levels and BTECs",
  "GCSEs and below", "International Baccalaureate", "Undergraduate",
  "Vocational Qualifications", "Unknown", "Other"
))

table(d$qualification_pathway_clean)


#### Other sports binary variables ####

# Create binary variables for each extra sport applicants do
table(d$other_sports)

sports_list <- c(`american_football` = "american football",
                 `football` = "football",
                 `tennis` = "tennis",
                 `swimming` = "swim",
                 `rugby` = "rugby",
                 `athletics` = "athletics",
                 `basketball` = "basketball",
                 `gymnastics` = "gymnastics|trampolining",
                 `badminton` = "badminton",
                 `golf` = "golf",
                 `squash` = "squash",
                 `cricket` = "cricket",
                 `hockey` = "hockey",
                 `cross_country` = "cross country",
                 `climbing` = "climbing",
                 `shooting` = "shooting",
                 `cycling_biking` = "cycl|biking|mtb|bmx",
                 `dance` = "dance",
                 `netball` = "netball",
                 `volleyball` = "volleyball",
                 `motorsports` = "karting",
                 `skiing` = "skiing",
                 `rounders` = "rounders",
                 `horse_riding` = "horse riding|show jumping",
                 `lacrosse` = "lacrosse",
                 `handball` = "handball",
                 `powerlifting` = "powerlifting",
                 `martial_arts` = "martial arts|karate|kick boxing|pencak silat|taekwondo|kung fu|judo",
                 `water_sports` = "surfing|paddling boarding|sailing|rowing|aquathlon",
                 `fencing` = "fencing",
                 `triathlon` = "triathlon",
                 `fitness_classes` = "fitness classes|yoga|pilates")

for (s in names(sports_list)) {
  pattern <- sports_list[[s]]
  d[[s]] <- ifelse(
    grepl(pattern, d$other_sports, ignore.case = TRUE),
    1,
    0
  )
}

d[names(sports_list)] <- lapply(d[names(sports_list)], function(x) {
  factor(x, levels = c(1, 0),
         labels = c("Yes", "No"))
})


#### Level of specialisation variable ####

# Level of specialisation variable, based on three y/n questions
# <=1 low
# 2 moderate
# 3 high
# Create a yes count variable, and then a specialisation variable
d <- d %>%
  mutate(
    yes_count = (main_sport == "Yes") + 
      (quit_sports == "Yes") + 
      (months_train8 == "Yes"),
    specialisation = case_when(
      yes_count <= 1 ~ "Low",
      yes_count == 2 ~ "Moderate",
      yes_count == 3 ~ "High",
      TRUE ~ "Unknown"
    )
  )

d$specialisation <- factor(d$specialisation,
                           levels = c("Low", "Moderate", "High", "Unknown"))
