#### Mapping ###################################################################

#### Set-up ####
library(sf)
library(dplyr)
library(ggplot2)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_clean.rds")

# Read in the shapefile downloaded from the ONS, I used the following link:
# https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2021-boundaries-ew-bsc-v4-2/about
lsoa_ONS <- st_read("LSOA_boundaries/LSOA_2021_EW_BSC_V4.shp")

str(lsoa_ONS)

# Slight exploration of LSOA_codes from dataset first
n_distinct(d$LSOA_code)
# 660 unique codes, since there are 691 applicants, 31 are from the same area
# as another applicant

# Therefore need to find counts for each LSOA
lsoa_counts <- d %>%
  count(LSOA_code)

# Seeing which places have multiple applicants from there
lsoa_counts %>%
  filter(n > 1)
# Maximum 2 from each of these places, except 5 applicants with missing values

# Join the the data with a left join, this will keep all the observations
# from the ONS lsoa dataframe, and will find matches with the lsoa_counts, 
# adding a new column with the number of applicants (and NAs for anywhere there
# was no applicant from)
map_data <- lsoa_ONS %>%
  left_join(lsoa_counts,
            by = c("LSOA21CD" = "LSOA_code"))

# Replace missing values
map_data$n[is.na(map_data$n)] <- 0


#### First map attempt ####
ggplot(map_data) +
  geom_sf(aes(fill = n), colour = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Applicants")


#### Disaggregating year groups and using points ###############################

# First just creating England map to map onto later
# Keep only English LSOAs
england_lsoa <- lsoa_ONS %>%
  filter(substr(LSOA21CD, 1, 1) == "E")

# Dissolve into a single England polygon
england <- england_lsoa %>%
  summarise()

# Just making an england and wales map too, see how it looks
ew_lsoa <- lsoa_ONS %>%
  filter(substr(LSOA21CD, 1, 1) %in% c("E", "W"))

england_wales <- ew_lsoa %>%
  summarise()

# 2021-2023 year group first, get all unique LSOA codes
participant_lsoas_2123 <- d %>%
  filter(year_group == "2021-23") %>%
  pull(LSOA_code) %>%
  unique()

# Find them in the ONS data
selected_lsoas_2123 <- lsoa_ONS %>%
  filter(LSOA21CD %in% participant_lsoas_2123)

# Find centroids
lsoa_points_2123 <- st_centroid(selected_lsoas_2123)

# Plot
ggplot() +
  geom_sf(data = england_wales,
          fill = "white",
          colour = "grey80") +
  geom_sf(data = lsoa_points_2123,
          colour = "#3B6FB6",
          size = 1.5) +
  ggtitle("2021-2023 Applicants") +
  theme_void()  +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "grey80",
      colour = NA
  )
)
 
# Same steps for 2022-2024 
participant_lsoas_2224 <- d %>%
  filter(year_group == "2022-24") %>%
  pull(LSOA_code) %>%
  unique()

selected_lsoas_2224 <- lsoa_ONS %>%
  filter(LSOA21CD %in% participant_lsoas_2224)

lsoa_points_2224 <- st_centroid(selected_lsoas_2224)

ggplot() +
  geom_sf(data = england,
          fill = "white",
          colour = "grey80") +
  geom_sf(data = lsoa_points_2224,
          colour = "#3B6FB6",
          size = 1.5) +
  ggtitle("2022-2024 Applicants") +
  theme_void()  +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "grey80",
      colour = NA
    )
  )


#### Attempt incorporating how many come from each area ########################

# First just want to see how many repeated LSOA codes in each year group,
# and missing values
d %>%
  group_by(year_group) %>%
  summarise(
    n_lsoas = n_distinct(LSOA_code),
    missing_lsoa = sum(is.na(LSOA_code))
  )
# of 310 applicants in 2021-23, 300 have unique codes, and 3 missing values
# of 381 applicants in 2022-24, 372 have unique codes, and 2 missing values

# Count number of applicants in each LSOA in 2021-23
lsoa_counts_2123 <- d %>%
  filter(year_group == "2021-23") %>%
  count(LSOA_code)
# max 2 in one LSOA code

# Count number of applicants in each LSOA in 2022-24
lsoa_counts_2224 <- d %>%
  filter(year_group == "2022-24") %>%
  count(LSOA_code)
# max 2 in one LSOA code

# join to ONS data and find centroids
lsoa_points_2123 <- lsoa_ONS %>%
  inner_join(
    lsoa_counts_2123,
    by = c("LSOA21CD" = "LSOA_code")
  ) %>%
  st_centroid()

lsoa_points_2224 <- lsoa_ONS %>%
  inner_join(
    lsoa_counts_2224,
    by = c("LSOA21CD" = "LSOA_code")
  ) %>%
  st_centroid()

# Checking because the dataframes have gotten smaller
# This code returns the rows in lsoa_counts_2123 that do not have a match in 
# the ONS data
anti_join(lsoa_counts_2123, lsoa_points_2123, by = c("LSOA_code" = "LSOA21CD"))
anti_join(lsoa_counts_2224, lsoa_points_2224, by = c("LSOA_code" = "LSOA21CD"))

# Convert to factor since max two people from each LSOA
lsoa_points_2123$n <- factor(lsoa_points_2123$n)
lsoa_points_2224$n <- factor(lsoa_points_2224$n)

ggplot() +
  geom_sf(
    data = england_wales,
    fill = "white",
    colour = "grey80"
  ) +
  ggtitle("2021-2023 Applicants") +
  geom_sf(
    data = lsoa_points_2123,
    aes(colour = n),
    size = 2
  ) +
  scale_colour_manual(
    values = c(
      "1" = "#619CFF",
      "2" = "#F8766D"
    ),
    name = "Applicants per LSOA"
  ) + 
  theme_void()   +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "grey80",
      colour = NA
    )
  )


ggplot() +
  geom_sf(
    data = england_wales,
    fill = "white",
    colour = "grey80"
  ) +
  ggtitle("2022-2024 Applicants") +
  geom_sf(
    data = lsoa_points_2224,
    aes(colour = n),
    size = 2
  ) +
  scale_colour_manual(
    values = c(
      "1" = "#619CFF",
      "2" = "#F8766D"
    ),
    name = "Applicants per LSOA"
  ) + 
  theme_void()   +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "grey80",
      colour = NA
    )
  )

#### Attempt with success rates ################################################

# Find proportion of succesful applicants in each LSOA code
lsoa_acceptance_2123 <- d %>%
  filter(year_group == "2021-23") %>%
  group_by(LSOA_code) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants
  )

lsoa_acceptance_2224 <- d %>%
  filter(year_group == "2022-24") %>%
  group_by(LSOA_code) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants
  )

# Join with ONS data
lsoa_points_2123 <- lsoa_ONS %>%
  inner_join(
    lsoa_acceptance_2123,
    by = c("LSOA21CD" = "LSOA_code")
  ) %>%
  st_centroid()

lsoa_points_2224 <- lsoa_ONS %>%
  inner_join(
    lsoa_acceptance_2224,
    by = c("LSOA21CD" = "LSOA_code")
  ) %>%
  st_centroid()

# Convert to factor (there are only three possible values: 0, 0.5, 1)
lsoa_points_2123$acceptance_rate <-
  factor(lsoa_points_2123$acceptance_rate)

lsoa_points_2224$acceptance_rate <-
  factor(lsoa_points_2224$acceptance_rate)

# Plot on map
ggplot() +
  geom_sf(
    data = england_wales,
    fill = "white",
    colour = "grey80"
  ) +
  geom_sf(
    data = lsoa_points_2123,
    aes(colour = acceptance_rate),
    size = 2
  ) + 
  theme_void()   +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "grey80",
      colour = NA
    )
  ) + 
  ggtitle("2021-2023 Applicants") +
  labs(colour = "Acceptance rate")

ggplot() +
  geom_sf(
    data = england_wales,
    fill = "white",
    colour = "grey80"
  ) +
  geom_sf(
    data = lsoa_points_2224,
    aes(colour = acceptance_rate),
    size = 2
  ) + 
  theme_void()   +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = "grey80",
      colour = NA
    )
  ) + 
  ggtitle("2022-2024 Applicants") +
  labs(colour = "Acceptance rate")
