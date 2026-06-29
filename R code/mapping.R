#### Mapping ###################################################################

#### Set-up ####
library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_clean.rds")

# Read in the shapefile downloaded from the ONS, I used the following link:
# https://geoportal.statistics.gov.uk/datasets/f23b8af6504640558a5100dfcd19a7ee_0/explore?location=52.837570%2C-2.489798%2C6
lsoa_ONS <- st_read("LSOA_boundaries2011/LSOA_2011_EW_BSC_V4.shp")

# Checking to see non-matches in the dataset with the ONS data
non_matches <-
  anti_join(d, lsoa_ONS, by = c("LSOA_code" = "LSOA11CD"))
# Only 11, comprising islands, france and missing values - 2011 is correct year

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
            by = c("LSOA11CD" = "LSOA_code"))

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
  filter(substr(LSOA11CD, 1, 1) == "E")

# Dissolve into a single England polygon
england <- england_lsoa %>%
  summarise()

# Making an england and wales map too, see how it looks
ew_lsoa <- lsoa_ONS %>%
  filter(substr(LSOA11CD, 1, 1) %in% c("E", "W"))

england_wales <- ew_lsoa %>%
  summarise()

# 2021-2023 year group first, get all unique LSOA codes
participant_lsoas_2123 <- d %>%
  filter(year_group == "2021-23") %>%
  pull(LSOA_code) %>%
  unique()

# Find them in the ONS data
selected_lsoas_2123 <- lsoa_ONS %>%
  filter(LSOA11CD %in% participant_lsoas_2123)

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
  filter(LSOA11CD %in% participant_lsoas_2224)

lsoa_points_2224 <- st_centroid(selected_lsoas_2224)

ggplot() +
  geom_sf(data = england_wales,
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
    by = c("LSOA11CD" = "LSOA_code")
  ) %>%
  st_centroid()

lsoa_points_2224 <- lsoa_ONS %>%
  inner_join(
    lsoa_counts_2224,
    by = c("LSOA11CD" = "LSOA_code")
  ) %>%
  st_centroid()

# Convert to factor since max two people from each LSOA
lsoa_points_2123$n <- factor(lsoa_points_2123$n)
lsoa_points_2224$n <- factor(lsoa_points_2224$n)

ggplot() +
  geom_sf(data = england_wales, fill = "white", colour = "grey80") +
  ggtitle("2021-2023 Applicants") +
  geom_sf(data = lsoa_points_2123, aes(colour = n), size = 2) +
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

# Find proportion of successful applicants in each LSOA code
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
    by = c("LSOA11CD" = "LSOA_code")
  ) %>%
  st_centroid()

lsoa_points_2224 <- lsoa_ONS %>%
  inner_join(
    lsoa_acceptance_2224,
    by = c("LSOA11CD" = "LSOA_code")
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


#### Upscaling to local authority district (LAD) ###############################

# Load LSOA to LAD lookup table, obtained from:
# https://geoportal.statistics.gov.uk/datasets/d382604321554ed49cc15dbc1edb3de3_0/explore
lookup <- read_csv("LSOA_to_LAD.csv") %>%
  select(LSOA11CD, LAD11CD, LAD11NM) %>%
  distinct()

# Load LAD boundary shapefile, obtained from:
# https://geoportal.statistics.gov.uk/datasets/4710f4b9f8db4a4fa3edff5bc886bccc_0/explore?location=52.837545%2C-2.489845%2C6
lad_ONS <- st_read("LAD_boundaries/Local_Authority_Districts_December_2011_FCB_EW.shp")

lad_d <- d %>%
  left_join(
    lookup,
    by = c("LSOA_code" = "LSOA11CD")
  )

# Find proportion of successful applicants in each LAD code
lad_acceptance_2123 <- lad_d %>%
  filter(year_group == "2021-23") %>%
  group_by(LAD11CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants
  )

lad_acceptance_2224 <- lad_d %>%
  filter(year_group == "2022-24") %>%
  group_by(LAD11CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants
  )

# Join with shapefile
lad_map2123 <- lad_ONS %>%
  left_join(lad_acceptance_2123, by = c("lad11cd" = "LAD11CD"))

lad_map2224 <- lad_ONS %>%
  left_join(lad_acceptance_2224, by = c("lad11cd" = "LAD11CD"))

# Plot map
ggplot(lad_map2123) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance rate") +
  ggtitle("Local Authority Aceptance Rates for 2021-2023 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA
        ))

ggplot(lad_map2224) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance rate") +
  ggtitle("Local Authority Aceptance Rates for 2022-2024 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA
  ))


#### Further upscaling to county/unitary authority district (CTYUA) level ######

# Load LSOA to CTYUA lookup table, obtained from:
# https://open-geography-portalx-ons.hub.arcgis.com/datasets/6f5221e8123a480883874849ddf5cbd8_0/explore
lookup2 <- read_csv("LSOA_to_CTYUA.csv") %>%
  select(LSOA11CD, CTYUA19CD, CTYUA19NM) %>%
  distinct()

# Load CTYUA boundary shapefile and limiting it to England and Wales, obtained from:
# https://geoportal.statistics.gov.uk/datasets/b31d8acce5b744c28f29e99a0df46491_0/explore?location=52.961464%2C-2.112839%2C6
ctyua_ONS <- st_read("CTYUA_boundaries/Counties_and_Unitary_Authorities_December_2019_FCB_UK.shp")  %>%
  filter(substr(ctyua19cd, 1, 1) %in% c("E", "W"))

# Join data and CTYUA codes
ctyua_d <- d %>%
  left_join(
    lookup2,
    by = c("LSOA_code" = "LSOA11CD")
  )

# Find proportion of successful applicants in each LAD code
ctyua_acceptance_2123 <- ctyua_d %>%
  filter(year_group == "2021-23") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants
  )

ctyua_acceptance_2224 <- ctyua_d %>%
  filter(year_group == "2022-24") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants
  )

# Join with shapefile
ctyua_map2123 <- ctyua_ONS %>%
  left_join(ctyua_acceptance_2123, by = c("ctyua19cd" = "CTYUA19CD"))

ctyua_map2224 <- ctyua_ONS %>%
  left_join(ctyua_acceptance_2224, by = c("ctyua19cd" = "CTYUA19CD"))

# Plot map
ggplot(ctyua_map2123) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance rate") +
  ggtitle("County and Unitary Authority District Rates for 2021-2023 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA
        ))

ggplot(ctyua_map2224) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance rate") +
  ggtitle("County and Unitary Authority District Rates for 2022-2024 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA
        ))


#### Checking number of applicants from each LAD/CTYUA #########################

lad_tab <- table(lad_d$LAD11NM)
table(lad_tab)

ctyua_tab <- table(ctyua_d$CTYUA19NM)
table(ctyua_tab)
