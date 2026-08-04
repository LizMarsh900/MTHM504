library(sf)
library(spdep)

# Create neighbourhood graph
lsoa <- st_read("LSOA_boundaries.gpkg")

nb <- poly2nb(lsoa)

nb2INLA("lsoa.graph", nb)

# or
graph <- inla.read.graph("lsoa.graph")

# Fit the model
library(INLA)

dat$area_id <- 1:nrow(dat)

model <- inla(
  applications ~
    f(area_id,
      model = "bym2",
      graph = graph),
  family = "poisson",
  data = dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

# Extract posterior estimates
dat$mean <- model$summary.random$area_id$mean
dat$lower <- model$summary.random$area_id$`0.025quant`
dat$upper <- model$summary.random$area_id$`0.975quant`

# Join these back to your polygons.
lsoa <- merge(lsoa, dat)

# Map the results
dat$prob_high <-
  1 - model$summary.random$area_id$`cdf0`