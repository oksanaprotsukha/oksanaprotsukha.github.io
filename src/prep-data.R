# load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(broom)
library(readr)
library(gt)
library(openintro)
library(ggplot2)
library(modelr)
library(lterdatasampler)
library(sf)
library(dplyr)
library(maptiles)
library(kableExtra)
library(janitor)

# load configuration
source(file.path(here::here('src','config.R')))

# retrieve API Key
api_key <- NREL_API_KEY

# get charging stations for United States
# specify API endpoint and parameters
api_url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1"

api_params <- list(
  format = "json",
  api_key = api_key,
  status = paste(c("E", "P"), collapse = ","), # E = charging stations in operation; P = planned
  access = "public",
  fuel_type = "ELEC",
  cng_vehicle_class = "LD", # LD = light duty vehicles
  country = "US",
  limit = "all"
)

# construct the full URL with parameters
request_url <- modify_url(api_url, query = api_params)

# make a GET request to the API
response <- GET(request_url)

# extract the fuel_stations data from the response
fuel_stations_us_df <- content(response, "parsed")$fuel_stations

# convert the list to a dataframe
fuel_stations_us_df <- as.data.frame(do.call(rbind, fuel_stations_us_df))

# select the fields for analysis
fuel_stations_us_df_clean <- fuel_stations_us_df %>%
  filter(!is.na(fuel_type_code)) %>%
  select('id', 'status_code', 'access_code','owner_type_code', 'open_date', 'restricted_access', 'maximum_vehicle_class','facility_type', 'city','state','zip','ev_workplace_charging', 'ev_level1_evse_num', 'ev_level2_evse_num', 'ev_dc_fast_num' , 'longitude', 'latitude')

# convert data frame to sf object
us_ev_sf <- fuel_stations_us_df_clean %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# read in shape file
cejst <- st_read(here::here("data","raw","/usa/usa.shp"))

# make an initial plot of cejst: California state
cejst %>%
  filter(SF == "California") %>%
  select(GEOID10) %>%
  ggplot() +
  geom_sf(size = 1.5, color = "black", fill = "#FFECB3") +
  ggtitle("CEJST: California") +
  coord_sf() +
  theme_minimal()

# select the fields for analysis
fuel_stations_us_df_clean <- fuel_stations_us_df %>%
  filter(!is.na(fuel_type_code)) %>%
  select('id', 'status_code', 'access_code','owner_type_code', 'open_date', 'restricted_access', 'maximum_vehicle_class','facility_type', 'city','state','zip','ev_workplace_charging', 'ev_level1_evse_num', 'ev_level2_evse_num', 'ev_dc_fast_num' , 'longitude', 'latitude')

# convert data frame to sf object
us_ev_sf <- fuel_stations_us_df_clean %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# select the fields for analysis
cejst_clean <- cejst %>%
  select(SF, CF, GEOID10, DSF_PFS, EBF_PFS, LMI_PFS, P100_PFS, P200_I_PFS, NPL_PFS, TSDF_PFS, TPF, TF_PFS, N_ENY, N_TRN, SN_C, PLHSE, EB_ET, DS_ET, TP_ET, POV_ET, IA_LMI_ET, IA_POV_ET, N_TRN_EOMI, FPL200S) %>%
  clean_names()

# compare CRS
# check cejst crs
st_crs(cejst_clean) # WGS 84; "EPSG",4326
st_crs(cejst_clean) == st_crs(us_ev_sf) # TRUE

# check invalid geometries
cejst_clean[!st_is_valid(cejst_clean), ]
us_ev_sf [!st_is_valid(us_ev_sf ), ]

# join ev charging and ejscreen datasets
us_ev_joined <- st_join(cejst_clean, us_ev_sf)

# verify the joined df
# dim(us_ev_sf %>% filter(status_code %in% c("E","P")))  # 61336    16
# dim(us_ev_joined %>% filter(status_code %in% c("E","P"))) # 61321    40

# drop geometries for further analysis
us_ev_df <- us_ev_joined %>%
  st_drop_geometry() %>%
  distinct() # remove duplicate rows

# convert NULL values to NAs
us_ev_df[us_ev_df == "NULL"] <- NA

# calculate number of charging stations per number of people in each census tract
# rough assumptions: we want 0.05% - 0.1% of charging ports per capita
# exclude level 1 ports from the dataset
# to simplify we are going to apply this ratio state-wide
us_ports_per_capita_excl_level1 <- us_ev_df %>%
  group_by(geoid10) %>%
  mutate(
    total_ports = any(!sapply(list(ev_dc_fast_num, ev_level2_evse_num, ev_level1_evse_num), is.na)),
    total_ports_per_tract = sum(
      ifelse(!is.na(ev_dc_fast_num), as.numeric(ev_dc_fast_num), 0),
      ifelse(!is.na(ev_level2_evse_num), as.numeric(ev_level2_evse_num), 0)
    ),
    total_ports_capita_pct = (sum(total_ports_per_tract) / sum(tpf, na.rm = TRUE)) * 100
  ) %>%
  ungroup() %>%
  distinct()

# remove extra variables
us_ports_per_capita_excl_level1_clean <- us_ports_per_capita_excl_level1 %>%
  select(sf, geoid10, id, status_code, total_ports, open_date, total_ports,
         total_ports_per_tract,  total_ports_capita_pct, tpf, tf_pfs, p100_pfs, sn_c)

# handle NA and Inf values
us_ports_per_capita_excl_level1_clean <- us_ports_per_capita_excl_level1_clean[is.finite(us_ports_per_capita_excl_level1_clean$total_ports_capita_pct), ]

# get the count of EV chargers per state
us_fast_ports_per_state <- us_ports_per_capita_excl_level1_clean %>%
  select(-id) %>%
  filter(! sf %in% c("Guam", "Northern Mariana Islands", "American Samoa", "Puerto Rico", "Virgin Islands")) %>%
  mutate(status_code = case_when(
    status_code == "E" ~ "Operational",
    status_code == "P" ~ "Planned",
    .default = "Unavailable"
  )) %>%
  distinct() %>%
  group_by(sf) %>%
  mutate(total_pop_per_state = sum(tpf),
         total_fast_ports_per_state = sum(total_ports_per_tract),
         total_fast_ports_per_pop = round((total_fast_ports_per_state/total_pop_per_state),8)) %>%
  ungroup() %>%
  group_by(sf, status_code) %>%
  mutate(total_fast_ports_per_pop_by_status = round((sum(sum(total_ports_per_tract))/sum(tpf)),8),
            total_fast_ports_by_status = (sum(total_ports_per_tract)),
            total_pop_per_state = unique(total_pop_per_state),
            total_fast_ports_per_pop = unique(total_fast_ports_per_pop),
            total_fast_ports_per_state = unique(total_fast_ports_per_state)) %>%
  ungroup() %>%
  distinct()

