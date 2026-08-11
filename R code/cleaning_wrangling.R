#### Set-up ####################################################################

# Load required libraries
library(excel.link) #for loading excel data with password protection
library(tidyverse) 
library(dplyr)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

  # Load data and assign to "d1", "d2" based on year group
# Used excel.link library for this, as it works with password protected documents
# Make sure the Excel file being loaded is NOT open when you run it
# See notes on coding
d1 <- xl.read.file("Data_revised_170726.xlsx",
                  password = "Sawe15930",           #OMIT IN WRITE-UP
                  write.res.password = "Sawe15930",
                  xl.sheet = "2021-23 (CLEAN)")

d2 <- xl.read.file("Data_revised_170726.xlsx",
                  password = "Sawe15930",
                  write.res.password = "Sawe15930",
                  xl.sheet = "2022-24 (CLEAN)")


#### Wrangling #################################################################

# There are duplicate columns both containing LSOA code so delete one
# This is necessary now because `rename` function won't work otherwise
d1$`LSOA code` <- NULL
d2$`LSOA code` <- NULL

# Join the data frames to create just one with all application years
# First rename variables that aren't labelled the same in each dataset
d2 <- d2 %>%
  rename(
    `Event Group:` = `Select Your Event Group:`,
    `Education/Work As of Sep 21:` = `As of September 2022, in terms of education/work -  please select which applies to you?`
  )

# Can now bind together to create one big usable dataset with all year groups
d <- bind_rows(d1, d2)

# Rename variables to make them more useable
d <- d %>%
  rename(
    `ID` = "Unique ID",
    `year_group` = "Year Group",
    `selected` = "YTP Selected",
    `centre_allocated` = "Centre Allocated",
    `centre_postcode` = "Centre Postcode",
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
    `future_involvement` = "Future Pathway Involvement",
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

# Remove rows with deleted data due to both duplicate applications and "other"
# and the applicants that were accepted without application
# Find these using the comment column
# If interested find out how many are being deleted:
# (Can remove and leave only one letter string to get specific counts)
d %>%
  filter(grepl("duplicate|deleted|accepted without application",
               Comment,
               ignore.case = TRUE)) %>%
  nrow()

# Now actually delete these rows
d <- d %>% 
  filter(!grepl("duplicate|deleted|Accepted without application", Comment,
                ignore.case = TRUE))

# Convert "Question omitted"s and "N/A"s into actual missing values in R
# First get column names:
vars <- names(d)

# Now apply function to each column - function converts specific strings into 
# null values in R
d[vars] <- lapply(d[vars], function(x) {
  x[x %in% c("Question omitted", "", "N/A")] <- NA
  x
})

# The NAs in disability column correspond to no disability so change this coding
# Same with future involvement variable
d <- d %>%
  mutate(disability = replace_na(disability, "No"),
         future_involvement = replace_na(future_involvement, "No"))

# Check structure of dataframe
str(d)

# Lots of variables need to be changed from character to factor
# Leaving out Id-type variables, and variables to be altered later
# Doing it manually so that the levels are ordered in a more meaningful way
# mostly making sure numerical things are ordered correctly,
# otherwise alphabetical and yes-no-unknown ordering
# First create a list with each factor variable and the ordered levels:
levels_list <- list(
  year_group = c("2021-23", "2022-24"),
  selected = c("Yes", "No"),
  centre_allocated = c("Bath", "Birmingham", "Leeds", "London",
                       "Loughborough", "Manchester"),
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
  health_problem = c("Yes", "No", "Don't know"),
  future_involvement = c("Yes", "No")
)

# Then apply function (making variables into factors) to the list
for (l in names(levels_list)) {
  d[[l]] <- factor(d[[l]], levels = levels_list[[l]])
}


#### Birth date quartiles ####

# Create birth date quartile variable 
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

# Wrangling work_education variable
# Drop down menu of options: State school/Independent school/Apprenticeship/
# Working/Other, where "Other" they wrote a response
# Check all responses to see what other things people wrote:
table(d$education_work)
# Mostly Colleges, a couple of grammar schools, and miscellaneous other stuff

# Create list for the for loop later, can be edited to incorporate more typos
education_work_list <- c(
  `State School` = "state school",
  `Independent School` = "independent school",
  `Apprenticeship` = "apprenticeship",
  `Working` = "working",
  `Grammar School` = "grammar",
  `College` = "college"
)

# Create default column
d$education_work_clean <- "Unknown/Other"

# For loop to create a new variable with cleaned education pathways
for (e in names(education_work_list)) {
  pattern <- education_work_list[[e]]
  d$education_work_clean[
    d$education_work_clean == "Unknown/Other" & #only rows labelled `Unknown/Other` can be updated
      grepl(pattern, d$education_work, ignore.case = TRUE)
  ] <- e
}

# Convert to newly cleaned education/work variable into a factor
d$education_work_clean <- factor(d$education_work_clean, levels = c(
  "Apprenticeship", "College", "Grammar School", "Independent School",
  "State School", "Working", "Unknown/Other"
))

# Sanity check - do the clean variable responses look right?
table(d$education_work_clean)


#### Religion #####

# Now checking religion variable same as education/work variable
# Check responses:
table(d$religion)
# Some people specifying Catholicism, which can come under Christianity

# Create a religion list (exact same process as before)
religion_list <- c(
  `Christian` = "catholic|christian"
)

# Create default column
d$religion_clean <- d$religion

# For loop to create a new variable with cleaned religions
for (r in names(religion_list)) {
  pattern <- religion_list[[r]]
  d$religion_clean[
      grepl(pattern, d$religion, ignore.case = TRUE)
  ] <- r
}

# Convert to factor - happy for levels to just be alphabetical
d$religion_clean <- as.factor(d$religion_clean)

# Sanity check
table(d$religion_clean)


#### Ethnicity ####

# Same process again for ethnicity, options were: White/Asian or Asian British
# Black or Black British/Mixed/Prefer not to say
# Check responses:
table(d$ethnicity)
# One person specified "White South African" which should come under "White"

# Create a ethnicity list (exact same process as before)
ethnicity_list <- c(
  `White (English/Welsh/Scottish/Northern Irish/British/Other)` = 
    "White South African"
)

# Create default column 
d$ethnicity_clean <- d$ethnicity

# For loop to create a new variable with cleaned ethnicity responses
for (e in names(ethnicity_list)) {
  pattern <- ethnicity_list[[e]]
  d$ethnicity_clean[
    grepl(pattern, d$ethnicity, ignore.case = TRUE)
  ] <- e
}

# Convert to factor - alphabetical but "prefer not to say" at the end
d$ethnicity_clean <- factor(d$ethnicity_clean, levels = c(
  "Asian or Asian British (Bangladeshi/Indian/Pakistani/Chinese/Other)",
  "Black or Black British (African/Caribbean/Other)",
  "Mixed (White & Black Caribbean/White & Black African/White & Asian/Other)",
  "White (English/Welsh/Scottish/Northern Irish/British/Other)",
  "Prefer not to say"
))

# Sanity check
table(d$ethnicity_clean)


#### Qualification pathway ####

# Same process for qualification pathway, options were: BTECs/A Levels/IB/NVQs/
# Apprenticeship/Other
table(d$qualification_pathway)
# lots of GCSEs, some combination qualifications, CTECs, T-levels, undergraduate

# Create a qualification list (exact same process as before)
qualification_list <- c(
  `GCSEs and below` = "GCSE|GSCE|Year 11|Year 9",
  `Vocational Qualifications` = 
    "CTEC|cambridge technical|t[ -]?levels?|dip(loma)?|level [23]|vocational",
  `Undergraduate` = "undergraduate",
  `Combined A-Levels and BTECs` = "a[ -]?levels?.*btec|btec.*a[ -]?levels?",
  `BTECs` = "btec",
  `A-Levels` = "a[ -]?levels?",
  `International Baccalaureate` = "International Baccalaureate",
  `Apprenticeship` = "apprenticeship"
)

# Create default column
d$qualification_pathway_clean <- "Unknown/Other"

# For loop to create a new variable with cleaned qualification pathways
for (q in names(qualification_list)) {
  pattern <- qualification_list[[q]]
  d$qualification_pathway_clean[
    d$qualification_pathway_clean == "Unknown/Other" & 
      grepl(pattern, d$qualification_pathway, ignore.case = TRUE)
  ] <- q
}

# Convert to factor - alphabetical order with "unknown" and "other" at end
d$qualification_pathway_clean <- 
  factor(d$qualification_pathway_clean, levels = c(
  "A-Levels", "Apprenticeship", "BTECs", "Combined A-Levels and BTECs",
  "GCSEs and below", "International Baccalaureate", "Undergraduate",
  "Vocational Qualifications", "Unknown/Other"
))

# Sanity check
table(d$qualification_pathway_clean)
# other consists of the BWFC scholarship again


#### Other sports binary variables ####

# Need to create binary variables for each extra sport applicants do
# First check responses:
table(d$other_sports)

# Create list of sports (can be edited for typos/new responses)
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

# For loop to create binary variables for each item in the sport list
for (s in names(sports_list)) {
  pattern <- sports_list[[s]]
  d[[s]] <- ifelse(
    grepl(pattern, d$other_sports, ignore.case = TRUE),
    1,
    0
  )
}

# Need to convert each new binary variable into a factor
# Apply factor function to each of the new variables
d[names(sports_list)] <- lapply(d[names(sports_list)], function(x) {
  factor(x, levels = c(1, 0),
         labels = c("Yes", "No"))
})

# Sanity check - check that new variables are all factors
str(d)


#### Level of specialisation variable ####

# Create level of specialisation variable, based on three y/n questions
# <=1 low
# 2 moderate
# 3 high
# Create a variable counting "yes"s, and then a specialisation variable
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

# Convert to factor - fairly obvious ordering here
d$specialisation <- factor(d$specialisation,
                           levels = c("Low", "Moderate", "High", "Unknown"))






# Save dataframe to save having to run through this every time
saveRDS(d, "data_revised_clean.rds")
