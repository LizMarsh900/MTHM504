#### Set-up ####
library(dplyr)
library(mice) #for multiple imputation

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_clean.rds")


#### Multiple Imputation #######################################################

# Level of specialisation variable created from 3 specific questions
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
  
  yes_count <- (d$main_sport == "Yes") +
    (d$quit_sports == "Yes") +
    (d$months_train8 == "Yes")
  
  d$specialisation <- case_when(
    yes_count == 3 ~ "High",
    yes_count == 2 ~ "Moderate",
    yes_count <= 1 ~ "Low"
  )
  
  d$specialisation <- factor(
    d$specialisation,
    levels = c("Low", "Moderate", "High")
  )
  
  d
})


#### Logistic Regression with imputed datasets #################################

# Setting reference categories 
d$specialisation <- relevel(d$ethnicity_clean, 
                            ref = "White (English/Welsh/Scottish/Northern Irish/British/Other)")

# Fitting model using specialisation so applying to each imputation
fits <- lapply(imp_d, function(d) {
  glm(selected ~ birth_quarter + specialisation + sex + ethnicity_clean + IMD_decile,
      family = binomial(link = "logit"),
      data = subset(d, year_group == "2021-23"))
})

pooled <- pool(as.mira(fits))

summary(pooled, conf.int = TRUE, exponentiate = TRUE)


