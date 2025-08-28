# notes ----

#1) Calculate biomass of pacific cod within imm snow crab 50th percentile home range 
#2) Calculate biomass of subarctic fish complex within imm snow crab 50th percentile home range

## LOAD PACKAGES + UNIVERSAL PARAMS --------
source("./Scripts/1) load_libs_params.R")

## LOAD DATA --------

  ## Groundfish specimen data
  gf_catch <- read.csv(paste0("./Data/", current.year, "/gf_cpue_timeseries.csv"))

  #Core immature snow crab stations
  imm_area <- read.csv(paste0("./Output/", current.year, "/imm_area_50perc.csv"))
  
  imm_stations <- pull(imm_area, STATION_ID)
  
## CALCULATE BIOMASS WITH IMM SNOW CRAB 50th PERCENTILE HOME RANGE --------

# Pacific cod -----
  # separate cod data
  gf_catch %>%
    filter(SPECIES_CODE %in% c(21720), # filter by cod
           STATIONID %in% imm_stations, # filter to snow crab core area
           YEAR >= 1988) %>% # filter by years where full station number in core area is sampled (174)
    group_by(YEAR) %>%
    reframe(mean_cod_CPUE = mean(log(CPUE_KGKM2+1))) -> mean_cod_cpue
  
  # Plot
  ggplot(mean_cod_cpue, aes(YEAR, mean_cod_CPUE)) +
    geom_line() +
    geom_point()+
    ggtitle(paste0("1988-", current.year, " cod biomass (log(CPUE_KGKM2+1))"))

# Arctic complex -----
  gf_catch %>%
    filter(SPECIES_CODE %in% c(10285,21725,10140,21348,10115,66045,24185,10212), # filter arctic complex (alaska plaice, arctic cod, bering flounder, butterfly sculpin, greenland turbot, marbled eelpout, wattled eelpout)
           STATIONID %in% imm_stations, # filter to snow crab core area
           YEAR >= 1988) %>% # filter by years where full station number in core area is sampled (174)
    group_by(YEAR, STATIONID) %>% 
    reframe(sum_CPUE = sum(CPUE_KGKM2)) %>% # sum by year and station
    group_by(YEAR) %>%
    reframe(mean_arctic_CPUE = mean(log(sum_CPUE+1))) -> mean_arctic_cpue

  
  ggplot(mean_arctic_cpue, aes(YEAR, mean_arctic_CPUE)) +
    geom_line() +
    geom_point() +
    ggtitle(paste0("1988-", current.year, " arctic biomass (log(CPUE_KGKM2+1))"))

# JOIN DATASETS and SAVE
mean_cod_cpue %>%
    left_join(mean_arctic_cpue) %>%
  rename(year = YEAR,
         `Pacific cod` = mean_cod_CPUE,
         `Arctic groundfish` = mean_arctic_CPUE) %>%
  pivot_longer(cols = -year) -> final

write.csv(final, paste0("./Output/", current.year, "/groundfish_mean_cpue.csv"))

  
  