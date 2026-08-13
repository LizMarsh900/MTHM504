#### Mapping learning process ##################################################


# Checking to see non-matches in the dataset with the ONS data
non_matches <-
  anti_join(d, lsoa_ONS, by = c("LSOA_code" = "LSOA11CD"))
# Only 11, comprising islands, france and missing values

#### First map attempt ####
ggplot(map_data) +
  geom_sf(aes(fill = n), colour = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Applicants")


#### Disaggregating year groups and using points #####
# Separating year-groups and using points (works better for tiny LSOAs)

# 2021-2023 year group first, get all unique LSOA codes
participant_lsoas_2123 <- d %>%
  filter(year_group == "2021-23") %>%
  pull(LSOA_code) %>%
  unique()

# Find LSOA codes in the ONS data
selected_lsoas_2123 <- lsoa_ONS %>%
  filter(LSOA11CD %in% participant_lsoas_2123)

# Find centroids of LSOA codes
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

#### Attempt incorporating how many come from each area ####
# Next I wanted to include counts of how many from each LSOA

# First just want to see how many repeated LSOA codes in each year group,
# and missing values
d %>%
  group_by(year_group) %>%
  summarise(
    n_lsoas = n_distinct(LSOA_code),
    missing_lsoa = sum(is.na(LSOA_code))
  )

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

#### Upscaling to local authority district (LAD) #####
# This is after mapping success rates, I wanted to go up from LSOA codes

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
  labs(fill = "Acceptance Rate") +
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
  labs(fill = "Acceptance Rate") +
  ggtitle("Local Authority Aceptance Rates for 2022-2024 Applicants") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(
          fill = "grey80",
          colour = NA
        ))







#### Log regression stuff ######################################################

#### No multiple imputation model (removed specialisation) #####

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
