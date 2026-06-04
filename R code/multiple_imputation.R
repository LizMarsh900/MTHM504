library(mice) #for multiple imputation

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