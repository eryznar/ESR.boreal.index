# notes ----

# Determine core habitat of immature snow crab in EBS
  #1) Determine stations that compose average core habitat across the long-term timeseries 
  #2) Determine average bottom temperature of core habitat within each yr
  
# Author: Erin Fedewa
#Last update: 7/31/2025 by ERR

#NOTES:
#Given that stations differed pre 1980, only 1980+ dataset was used to calculate 
  #avg core habitat across timeseries
#Bottom temps need to be data corrected and imputed for final timeseries dataset

source("./Scripts/1) load_libs_params.R")

## EBS catch and haul data ----
sc_dat <- crabpack::get_specimen_data(species = "SNOW", 
                                        region = "EBS", 
                                        years = c(1975:2019, 2021:current.year),
                                        channel = "API")
sc_catch <- sc_dat$specimen
sc_haul <- sc_dat$haul

###################################################
# data exploration ----

#Stations sampled in each year
sc_catch %>%
  group_by(YEAR) %>%
  summarise(num_stations = length(unique(STATION_ID)))

#Plot pre-standardization data
sc_catch %>%
  filter(YEAR < 1988) %>%
  group_by(YEAR, LONGITUDE, LATITUDE) %>%
  distinct(STATION_ID) %>%
ggplot() +
  geom_point(aes(x =LONGITUDE, y = LATITUDE), size=.5) +
  labs(x = "Longitude", y = "Latitude") +
  facet_wrap(~YEAR)

#Earliest yrs don't survey prime snow crab habitat so 90% of habitat index
  #will be biased in these years 

#Calculate CPUE by station for all immature snow crab 
cpue <- crabpack::calc_cpue(crab_data = sc_dat,
                  species = "SNOW",
                  crab_category = c("all_categories"), # error when specifying small_male and immature_female directly
                  rm_corners = TRUE, # exclude corner stations
                  years = c(1975:2019, 2021:current.year),
                  region = "EBS",
                  shell_condition = c("soft_molting", "new_hardshell")) %>% # SC =<2
        filter(CATEGORY %in% c("small_male", "immature_female")) %>% # only immature
        group_by(YEAR, STATION_ID, LATITUDE, LONGITUDE) %>%
        reframe(CPUE = sum(CPUE))

# Determine stations that compose average core habitat across the long-term timeseries 
#stations in 50-100 CPUE percentile range
cpue %>%
  group_by(STATION_ID) %>%
  summarise(AVG_CPUE = mean(CPUE)) %>%
  filter(AVG_CPUE > quantile(AVG_CPUE, 0.50)) -> perc50 #174 stations
#Lets go with the 50th percentile for defining core immature area 

#Join lat/long back in to perc50 dataset and plot
cpue %>%
      filter(YEAR == 2021) %>% #Just selecting a yr when all stations were sampled
      dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
      right_join(perc50) -> perc50_core

#Write csv for stations in 50th percentile of avg CPUE  
write.csv(perc50_core, file=paste0("./Output/", current.year, "/imm_area_50perc.csv"))
