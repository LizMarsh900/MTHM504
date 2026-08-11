#### Set-up ####
library(dplyr)
library(tidyverse)
library(mice) #for multiple imputation
library(flextable)
library(officer)

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
            m = 5,
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

pooled <- pool(as.mira(fits))

results <- summary(pooled, conf.int = TRUE, exponentiate = TRUE)

# Best model according to stepwise selection:
# selected ~ sex + ethnicity_clean + qualification_pathway_clean + 
#  years_competing + primary_event + health_problem

#### No multiple imputation model (removed specialisation) #####################

d_complete <- na.omit(
  d[, c("selected", "birth_quarter", "year_group", 
          "sex", "disability","ethnicity_clean", "IMD_decile", "religion_clean",
          "education_work_clean", "qualification_pathway_clean", 
          "years_competing", "primary_event", 
          "hours_week", "specialist_support", "health_problem")]
)

mod <- glm(selected ~ birth_quarter*year_group + 
      sex + disability + ethnicity_clean + IMD_decile + religion_clean +
      education_work_clean + qualification_pathway_clean + 
      years_competing + primary_event + 
      hours_week + specialist_support + health_problem,
    family = binomial(link = "logit"),
    data = d_complete)

null_mod <- glm(selected ~ 1, family = binomial(link = "logit"), data = d_complete)

anova(null_mod, mod, test = "Chisq")


#### Model fit assessment ######################################################

fit_null <- with(
  imp,
  glm(selected ~ 1, family = binomial)
)

D1(fits, fit_null)

#### Neat results ##############################################################

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

names(results) <- c(
  "Predictor",
  "OR",
  "SE",
  "p",
  "CI_lower",
  "CI_upper"
)

results$OR <- round(results$OR, 2)
results$SE <- round(results$SE, 2)
results$CI_lower <- round(results$CI_lower, 2)
results$CI_upper <- round(results$CI_upper, 2)
results$p <- round(results$p, 3)

+results$Predictor <- c(
  "Birth quarter 2",
  "Birth quarter 3",
  "Birth quarter 4",
  "Year group: 2022–24",
  "Birth quarter 2 × year group: 2022–24",
  "Birth quarter 3 × year group: 2022–24",
  "Birth quarter 4 × year group: 2022–24"
)

results$`95% CI` <- paste0(
  results$CI_lower,
  ", ",
  results$CI_upper
)
  
results <- results %>% subset(select = -c(CI_lower, CI_upper))

ft <- flextable(results)
doc <- read_docx()
doc <- body_add_flextable(doc, ft)

print(doc, target = "logreg_table.docx")
