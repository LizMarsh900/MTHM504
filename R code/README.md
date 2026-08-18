# R scripts

R code used for cleaning and wrangling the data. I used separate scripts for separate aims. These include:

- Cleaning and wrangling file: this gets the data ready to use by neatening it in lots of ways. For example: cleaning variables in which applicants had written responses that differed from the drop-down options, adding new variables such as binary variables for each "other sport" mentioned or level of specialisation, renaming variables, recoding missing values or creating missing values, removing duplicated and deleted applicants etc.
- EDA file: exploration of the data. This mostly was used to create demographics tables showing proportions/numbers/averages of each variable. Some graphical exploration of variables too
- Mapping file: this contains code that was used to create the maps of England showing which LSOA each of the applicants is from. I also mapped success rates at the County and Unitary Authority (CTYUA) district level and created BYM2 model to model application counts at the CTYUA level
- Chi square file: contains the code used to compare birth distributions in the YTP application pool to the general population
- Logistic regression: used to assess whether birth quarter impacted the odds of selection to the YTP, also used a multiple imputation procedure
- Extra stuff: I moved some stuff out of the other files if it wasn't particularly relevant to the project/analysis. For example, my process of learning how to map is in here
