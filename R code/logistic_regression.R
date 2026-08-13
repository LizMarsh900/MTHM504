#### Set-up ####
library(dplyr)
library(tidyverse)
library(mice) #for multiple imputation
library(flextable)
library(officer)
library(pROC)
library(performance)
library(see)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_revised_clean.rds")


#### Multiple Imputation #######################################################

# Level of specialisation variable created from 3 specific questions
# first 54 applicants from 2022-24 cohort have missing data for these Qs
# Use multiple imputation for these

# First check missingness
sum(is.na(d$main_sport)) #52
sum(is.na(d$quit_sports)) #52
sum(is.na(d$months_train8)) #0 - so actually no missing data here

# Set up imputation method, and set it to blank for every variable
method <- make.method(d)
method[] <- ""

# Set up which variables to impute - use log regression - best for y/n outcomes
method["main_sport"] <- "logreg"
method["quit_sports"] <- "logreg"

# Impute missing data for the 2 questions using mice
imp <- mice(d,
            m = 30,
            method = method,
            seed = 123)

# Extract imputed datasets
imp_d <- complete(imp, action = "all")

# Create specialisation variable in each of the imputed datasets
# First count yeses, then specialisation variable, then convert to factor
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

# Fitting model using specialisation so applying to each imputation
fits <- lapply(imp_d, function(d) {
  glm(selected ~ birth_quarter*year_group + 
        sex + disability + ethnicity_clean + IMD_decile + religion_clean +
        education_work_clean + qualification_pathway_clean + 
        years_competing + specialisation + primary_event + 
        hours_week + specialist_support + health_problem,
      family = binomial(link = "logit"),
      data = d)
})
# Note: health problem variable causes issues here - three people responded "Don't know"
# and all three were selected, causing complete separation. it's not a variable of interest 
# so i'm just not interpreting their coefficients but future analysis should probably attempt to remedy this
# Could use multiple imputation for the "don't knows"
# Same could be done for any "unknown/other" in other variables

# Pool results using Rubin's rules
pooled <- pool(as.mira(fits))

# Get a summary of pooled results
results <- summary(pooled, conf.int = TRUE, exponentiate = TRUE)


#### Model checking ############################################################

# Random selection of the 30 imputed models
# 29, 24, 1 (from random number generator)


# Just creating a complete cases dataset to use for the null model
d29 <- imp_d[[29]]

d29_complete <- d29[complete.cases(
  d29[, c("selected", "birth_quarter", "year_group", "sex", "disability", 
          "ethnicity_clean", "IMD_decile", "religion_clean",
          "education_work_clean", "qualification_pathway_clean", 
          "years_competing", "specialisation", "primary_event", 
          "hours_week", "specialist_support", "health_problem")]
), ]

# Likelihood-ratio test
null_model <- glm(selected ~ 1, data = d29_complete, family = binomial)
anova(null_model, fits[[29]], test = "Chisq")


# same again
d24 <- imp_d[[24]]

d24_complete <- d24[complete.cases(
  d24[, c("selected", "birth_quarter", "year_group", "sex", "disability", 
          "ethnicity_clean", "IMD_decile", "religion_clean",
          "education_work_clean", "qualification_pathway_clean", 
          "years_competing", "specialisation", "primary_event", 
          "hours_week", "specialist_support", "health_problem")]
), ]

null_model <- glm(selected ~ 1, data = d24_complete, family = binomial)
anova(null_model, fits[[24]], test = "Chisq")

# same again
d1 <- imp_d[[21]]

d1_complete <- d1[complete.cases(
  d1[, c("selected", "birth_quarter", "year_group", "sex", "disability", 
          "ethnicity_clean", "IMD_decile", "religion_clean",
          "education_work_clean", "qualification_pathway_clean", 
          "years_competing", "specialisation", "primary_event", 
          "hours_week", "specialist_support", "health_problem")]
), ]

null_model <- glm(selected ~ 1, data = d1_complete, family = binomial)
anova(null_model, fits[[1]], test = "Chisq")

# Pooled overall model comparison
null_fits <- lapply(imp_d, function(d) {
  glm(
    selected ~ 1,
    family = binomial,
    data = d
  )
})
D1(fits, null_fits)


# Checking ROC AND AUC for all imputed models
aucs <- sapply(fits, function(m) {
  roc(
    model.response(model.frame(m)),
    fitted(m),
    direction = "<"
  )$auc
})

aucs

# Overall diagnostic plots
check_model(fits[[29]])
check_model(fits[[24]])
check_model(fits[[1]])


#### Neat results for write-up #################################################

# Select only the results for predictors relevant to my project
# And remove unneccessary columns
results <- results[results$term %in% c(
  "birth_quarter2",
  "birth_quarter3",
  "birth_quarter4",
  "year_group2022-24",
  "birth_quarter2:year_group2022-24",
  "birth_quarter3:year_group2022-24",
  "birth_quarter4:year_group2022-24"
  ),
] %>% subset(select = -c(statistic, df, conf.low, conf.high))

# Rename columns
names(results) <- c(
  "Predictor",
  "OR",
  "SE",
  "p",
  "CI_lower",
  "CI_upper"
)

# Round all the results
results$OR <- round(results$OR, 2)
results$SE <- round(results$SE, 2)
results$CI_lower <- round(results$CI_lower, 2)
results$CI_upper <- round(results$CI_upper, 2)
results$p <- round(results$p, 3)

# Rename predictors so it's clearer in the final table
results$Predictor <- c(
  "Birth quarter 2",
  "Birth quarter 3",
  "Birth quarter 4",
  "Year group: 2022–24",
  "Birth quarter 2 × year group: 2022–24",
  "Birth quarter 3 × year group: 2022–24",
  "Birth quarter 4 × year group: 2022–24"
)

# Combine CIs into one column, with each number having a comma between them
results$`95% CI` <- paste0(
  results$CI_lower,
  ", ",
  results$CI_upper
)

# Get rid of CI columns since we have a new one 
results <- results %>% subset(select = -c(CI_lower, CI_upper))

# Convert to a flextable and create a word document with it in
ft <- flextable(results)
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "logreg_table.docx")
