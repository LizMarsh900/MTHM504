# Load required libraries
library(tidyverse)
library(sf)
library(spdep)
library(INLA)

# NOTE: INLA cannot be installed easily  through `install.packages`. Use this:
install.packages("INLA",
                 repos = c(getOption("repos"),
                           INLA = "https://inla.r-inla-download.org/R/stable"),
                 dependencies = TRUE)

# Set working directory  to folder with data - if not already done
setwd("C:/Users/Liz/OneDrive - University of Exeter/MSc/Summer Project/data_copy")

# Load clean data (if not already loaded)
d <- readRDS("data_revised_clean.rds")


#### Process borrowed from mapping code ########################################

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

# Find number of applications in each CTYUA code
ctyua_applications_2123 <- ctyua_d %>%
  filter(year_group == "2021-23") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applications = n()
  )

ctyua_applications_2224 <- ctyua_d %>%
  filter(year_group == "2022-24") %>%
  group_by(CTYUA19CD) %>%
  summarise(
    applications = n()
  )


#### Smoothing process 2021-23 #################################################

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
d2123$lower <- model2123$summary.linear.predictor$`0.025quant`
d2123$upper <- model2123$summary.linear.predictor$`0.975quant`

# Extract posterior estimates - fitted values on original scale:
d2123$smoothed_applications <- model2123$summary.fitted.values$mean

# Join these back to your polygons.
ctyua_results2123 <- ctyua_ONS %>%
  left_join(d2123, by = "ctyua19cd")

# Plot expected number of applications
ggplot(ctyua_results2123) +
  geom_sf(aes(fill = smoothed_applications),
          colour = "white",
          linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Expected\napplications"
  ) +
  labs(
    title = "Smoothed application intensity",
    subtitle = "Posterior mean from BYM2 model"
  ) +
  theme_minimal()
