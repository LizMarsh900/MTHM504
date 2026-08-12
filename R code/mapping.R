#### Mapping ###################################################################

#### Set-up ####
library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(PostcodesioR) # For getting lat and lon of postcodes
library(spdep) 
library(INLA) #For spatial smoothing later on
library(showtext) #For getting Times New Roman font on figures

#### Only needs to be run once ####

# NOTE: INLA cannot be installed easily  through `install.packages`. Use this:
install.packages("INLA",
                 repos = c(getOption("repos"),
                           INLA = "https://inla.r-inla-download.org/R/stable"),
                 dependencies = TRUE)

# Get Times New Roman
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
showtext_auto()

#####

# Set working directory  to folder with data - change as necessary
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_revised_clean.rds")

# Read in the shapefile downloaded from the ONS, I used the following link:
# https://geoportal.statistics.gov.uk/datasets/f23b8af6504640558a5100dfcd19a7ee_0/explore?location=52.837570%2C-2.489798%2C6
lsoa_ONS <- st_read("LSOA_boundaries2011/LSOA_2011_EW_BSC_V4.shp")

# Checking to see non-matches in the dataset with the ONS data
non_matches <-
  anti_join(d, lsoa_ONS, by = c("LSOA_code" = "LSOA11CD"))
# Only 11, comprising islands, france and missing values

# Slight exploration of LSOA_codes from dataset first - how many unique codes:
n_distinct(d$LSOA_code)

# Fewer codes than applicants, therefore need to find counts for each LSOA
lsoa_counts <- d %>%
  count(LSOA_code)

# Which LSOAs have multiple applicants from there:
lsoa_counts %>%
  filter(n > 1)
# Maximum 2 from each of these places, except 5 applicants with missing values


# First just creating England map to map onto later - keep only English LSOAs
england_lsoa <- lsoa_ONS %>%
  filter(substr(LSOA11CD, 1, 1) == "E")

# Dissolve into a single England polygon
england <- england_lsoa %>%
  summarise()

# Making an england and wales map too, see how it looks (same steps)
ew_lsoa <- lsoa_ONS %>%
  filter(substr(LSOA11CD, 1, 1) %in% c("E", "W"))

england_wales <- ew_lsoa %>%
  summarise()


#### Maps with success rates ###################################################

# Find proportion of successful applicants in each LSOA code (2021-23)
lsoa_acceptance_2123 <- d %>%
  filter(year_group == "2021-23") %>%
  group_by(LSOA_code) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants)

# Join with ONS data
lsoa_points_2123 <- lsoa_ONS %>%
  inner_join(
    lsoa_acceptance_2123,
    by = c("LSOA11CD" = "LSOA_code")) %>%
  st_centroid()

# Convert to factor (there are only three possible values: 0, 0.5, 1)
lsoa_points_2123$acceptance_rate <-
  factor(lsoa_points_2123$acceptance_rate)

# Plot on map
ggplot() +
  geom_sf(data = england_wales, fill = "white", colour = "grey80") +
  geom_sf(data = lsoa_points_2123, aes(colour = acceptance_rate), size = 2) + 
  theme_void()   +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "grey80", colour = NA)) + 
  ggtitle("Lower-Layer Super Output Area Acceptance Rates \nfor 2021-2023 Applicants") +
  labs(colour = "Acceptance Rate")


#### Same steps again for 2022-2024 year-group ####
lsoa_acceptance_2224 <- d %>%
  filter(year_group == "2022-24") %>%
  group_by(LSOA_code) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants)

lsoa_points_2224 <- lsoa_ONS %>%
  inner_join(
    lsoa_acceptance_2224,
    by = c("LSOA11CD" = "LSOA_code")) %>%
  st_centroid()

lsoa_points_2224$acceptance_rate <-
  factor(lsoa_points_2224$acceptance_rate)

ggplot() +
  geom_sf(data = england_wales, fill = "white", colour = "grey80") +
  geom_sf(data = lsoa_points_2224, aes(colour = acceptance_rate), size = 2) + 
  theme_void()   +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "grey80", colour = NA)) + 
  ggtitle("Lower-Layer Super Output Area Acceptance Rates \nfor 2022-2024 Applicants") +
  labs(colour = "Acceptance Rate")


#### Upscaling to county/unitary authority district (CTYUA) level ######

# Load LSOA to CTYUA lookup table, obtained from:
# https://open-geography-portalx-ons.hub.arcgis.com/datasets/6f5221e8123a480883874849ddf5cbd8_0/explore
lookup <- read_csv("LSOA_to_CTYUA.csv") %>%
  select(LSOA11CD, CTYUA19CD, CTYUA19NM) %>%
  distinct()

# Load CTYUA boundary shapefile and limiting it to England and Wales, obtained from:
# https://geoportal.statistics.gov.uk/datasets/b31d8acce5b744c28f29e99a0df46491_0/explore?location=52.961464%2C-2.112839%2C6
ctyua_ONS <- st_read("CTYUA_boundaries/Counties_and_Unitary_Authorities_December_2019_FCB_UK.shp")  %>%
  filter(substr(ctyua19cd, 1, 1) %in% c("E"))

# Join data and CTYUA codes
ctyua_d <- d %>%
  left_join(
    lookup,
    by = c("LSOA_code" = "LSOA11CD")
  )

# Find proportion of successful applicants in each CTYUA code (2021-2023)
ctyua_acceptance_2123 <- ctyua_d %>%
  filter(year_group == "2021-23") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants)

# Join with shapefile
ctyua_map2123 <- ctyua_ONS %>%
  left_join(ctyua_acceptance_2123, by = c("ctyua19cd" = "CTYUA19CD"))

# Plot map
ggplot(ctyua_map2123) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance Rate") +
  ggtitle("County and Unitary Authority District Acceptance Rates \nfor 2021-2023 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA))

#### Same steps for 2022-2024 ####
ctyua_acceptance_2224 <- ctyua_d %>%
  filter(year_group == "2022-24") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants)

ctyua_map2224 <- ctyua_ONS %>%
  left_join(ctyua_acceptance_2224, by = c("ctyua19cd" = "CTYUA19CD"))

ggplot(ctyua_map2224) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance \nRate") +
  ggtitle("County and Unitary Authority District Acceptance Rates \nfor 2022-2024 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Times New Roman"),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA))


#### Learning how to drop points for regional hubs #############################

# Create a tibble with the names and postcodes of hubs
locations <- tibble(
  name = c("London", "Leeds", "Loughborough", "Birmingham", "Manchester", "Bath"),
  postcode = c("UB8 3PH", "LS6 3QQ", "LE11 3TP", "B15 2TT", "M11 3FF", "BA2 7AY"))

# Use postcode_lookup to obtain lat and lons for each hub and convert to sf point
hubs <- locations %>%
  mutate(pc = map(postcode, postcode_lookup),
         lon = map_dbl(pc, "longitude"),
         lat = map_dbl(pc, "latitude")) %>%
  select(-pc) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(ctyua_map2224))

# Add dummy column for hubs df so that a legend can be created on figures
hubs$hub_type <- "Regional Hubs"

# Same CTYUA map as earlier but with all hubs on as an example
ggplot(ctyua_map2123) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance Rate") +
  ggtitle("County and Unitary Authority District Aceptance Rates for 2021-2023 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA
        )) +
  geom_sf(data = hubs, colour = "red", size = 4)


#### Combined year group plots for write-up ####
ctyua_acceptance <- ctyua_d %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applicants = n(),
    accepted = sum(selected == "Yes"),
    acceptance_rate = accepted / applicants)

ctyua_map <- ctyua_ONS %>%
  left_join(ctyua_acceptance, by = c("ctyua19cd" = "CTYUA19CD"))

ggplot(ctyua_map) +
  geom_sf(aes(fill = acceptance_rate), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Acceptance \nRate") +
  ggtitle("County and Unitary Authority District Acceptance Rates") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        text = element_text(family = "Times New Roman", size = 16),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA), 
        legend.position = "left", legend.box.margin = margin(0, 0, 0, 30))  +
  geom_sf(data = hubs, aes(shape = hub_type) ,colour = "black", 
          fill = "red", size = 4) +
  scale_shape_manual(
    name = NULL,
    values = c("Regional Hubs" = 24)
  )

#### Total map with LSOA centroids plotted for reference ####

# Count number of applicants in each LSOA
lsoa_counts<- d %>%
  count(LSOA_code)

# join to ONS data and find centroids
lsoa_points <- lsoa_ONS %>%
  inner_join(
    lsoa_counts,
    by = c("LSOA11CD" = "LSOA_code")
  ) %>%
  st_centroid()

# Convert to factor since max two people from each LSOA
lsoa_points$n <- factor(lsoa_points$n)

# Plot
ggplot() +
  geom_sf(data = england, fill = "white", colour = "grey80") +
  ggtitle("Applicant Lower Layer Super Output Areas") +
  geom_sf(data = lsoa_points, aes(colour = n), size = 2, alpha = 0.7) +
  scale_colour_manual(
    values = c(
      "1" = "#619CFF",
      "2" = "#F8766D"
    ),
    name = "Applicants \nper LSOA"
  ) + 
  theme_void()   +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        text = element_text(family = "Times New Roman", size = 16),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA),
        legend.position = "left", legend.box.margin = margin(0, 0, 0, 30)
  )

#### SPATIAL SMOOTHING #########################################################

#### First mapping raw application counts ####

# Find number of applications in each CTYUA code (2021-23)
ctyua_applications_2123 <- ctyua_d %>%
  filter(year_group == "2021-23") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applications = n())

# Map raw application counts for comparison later
ctyua_rawmap2123 <- ctyua_ONS %>%
  left_join(ctyua_applications_2123, by = c("ctyua19cd" = "CTYUA19CD"))

# Plot map
ggplot(ctyua_rawmap2123) +
  geom_sf(aes(fill = applications), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Applications") +
  ggtitle("County and Unitary Authority District Application Counts \nfor 2021-2023 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA)) +
  geom_sf(data = hubs, colour = "red", size = 4)

### Same process for 2022-2024 ####
ctyua_applications_2224 <- ctyua_d %>%
  filter(year_group == "2022-24") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applications = n())

ctyua_rawmap2224 <- ctyua_ONS %>%
  left_join(ctyua_applications_2224, by = c("ctyua19cd" = "CTYUA19CD"))

ggplot(ctyua_rawmap2224) +
  geom_sf(aes(fill = applications), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Applications") +
  ggtitle("County and Unitary Authority District Application Counts \nfor 2022-2024 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA)) +
  geom_sf(data = hubs, colour = "red", size = 4)


#### Raw application counts for whole cohort ####
ctyua_applications <- ctyua_d %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applications = n())

ctyua_rawmap <- ctyua_ONS %>%
  left_join(ctyua_applications, by = c("ctyua19cd" = "CTYUA19CD"))

ggplot(ctyua_rawmap) +
  geom_sf(aes(fill = applications), color = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  theme_void() +
  labs(fill = "Applications") +
  ggtitle("County and Unitary Authority District Application Counts") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        text = element_text(family = "Times New Roman", size = 16),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA), 
        legend.position = "left", legend.box.margin = margin(0, 0, 0, 30))


#### Smoothing process 2021-23 #####

# Number of applications per CTYUA INCLUDING CTYUA CODES WITH ZERO APPLICANTS
d2123 <- ctyua_ONS %>%
  st_drop_geometry() %>%
  select(ctyua19cd) %>%
  left_join(ctyua_applications_2123,
            by = c("ctyua19cd" = "CTYUA19CD")) %>%
  mutate(applications = replace_na(applications, 0))

# Create neighbourhood list i.e. which counties share boundaries
nb <- poly2nb(ctyua_ONS)

# Write graph for INLA format
nb2INLA("ctyua.graph", nb)

# Read graph into INLA
graph2123 <- inla.read.graph("ctyua.graph")

# Fit the model
d2123$area_id <- 1:nrow(d2123)

model2123 <- inla(
  applications ~
    f(area_id,
      model = "bym2",
      graph = graph2123),
  family = "poisson",
  data = d2123,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

# Extract posterior estimates - on the log scale:
d2123$mean <- model2123$summary.linear.predictor$mean
d2123$lower <- model2123$summary.linear.predictor$`0.025quant` # CIs
d2123$upper <- model2123$summary.linear.predictor$`0.975quant`

# Extract posterior estimates - on original scale:
d2123$smoothed_applications <- model2123$summary.fitted.values$mean
d2123$credible_lower <- model2123$summary.linear.predictor$`0.025quant` # Credible intervals
d2123$credible_upper <- model2123$summary.linear.predictor$`0.975quant`

# Join these back to polygons
ctyua_results2123 <- ctyua_ONS %>%
  left_join(d2123, by = "ctyua19cd")

# Plot expected number of applications
ggplot(ctyua_results2123) +
  geom_sf(aes(fill = smoothed_applications), colour = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  labs(title = "Smoothed Expected Application Counts \nfrom BYM2 Model (2021-2023)",
    fill = "Expected\napplications") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA)) 


#### Smoothing process for whole cohort ####

# Number of applications per CTYUA INCLUDING CTYUA CODES WITH ZERO APPLICANTS
d_smooth <- ctyua_ONS %>%
  st_drop_geometry() %>%
  select(ctyua19cd) %>%
  left_join(ctyua_applications,
            by = c("ctyua19cd" = "CTYUA19CD")) %>%
  mutate(applications = replace_na(applications, 0))

# Create neighbourhood list i.e. which counties share boundaries
nb <- poly2nb(ctyua_ONS)

# Write graph for INLA format
nb2INLA("ctyua.graph", nb)

# Read graph into INLA
graph <- inla.read.graph("ctyua.graph")

# Fit the model
d_smooth$area_id <- 1:nrow(d_smooth)

model <- inla(
  applications ~
    f(area_id,
      model = "bym2",
      graph = graph),
  family = "poisson",
  data = d_smooth,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

# Extract posterior estimates - on the log scale:
d_smooth$mean <- model$summary.linear.predictor$mean
d_smooth$lower <- model$summary.linear.predictor$`0.025quant` # CIs
d_smooth$upper <- model$summary.linear.predictor$`0.975quant`

# Extract posterior estimates - on original scale:
d_smooth$smoothed_applications <- model$summary.fitted.values$mean
d_smooth$credible_lower <- model$summary.fitted.values$`0.025quant` # Credible intervals
d_smooth$credible_upper <- model$summary.fitted.values$`0.975quant`

# Find observed application count minus fitted
# then positive numbers mean more applications observed than predicted
# negative means fewer applications observed than predicted
d_smooth$difference <- d_smooth$applications - d_smooth$smoothed_applications

# Join these back to polygons
ctyua_results <- ctyua_ONS %>%
  left_join(d_smooth, by = "ctyua19cd")

# Plot expected number of applications
ggplot(ctyua_results) +
  geom_sf(aes(fill = difference), colour = NA) +
  scale_fill_viridis_c(option = "D", na.value = "white") +
  labs(title = "Observed - Fitted Application Counts",
       fill = "Observed -\nFitted") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        text = element_text(family = "Times New Roman", size = 16),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA), 
        legend.position = "left", legend.box.margin = margin(0, 0, 0, 30))

# Find hyperparameters (e.g. overall spatial effect)
model$summary.hyperpar
