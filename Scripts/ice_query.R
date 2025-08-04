# PURPOSE: To produce mean Jan-Feb and Mar-Apr sea ice cover for the Bering

# AUTHOR: Emily Ryznar

# 1) LOAD LIBS/PARAMS ----
  
source("./Scripts/load_libs_params.R")

ice.years <- 1952:current.year

# 2) DOWNLOAD ICE DATA FROM ERA5 (can save each year, but running all years doesn't take long) ----
  # source: https://cds.climate.copernicus.eu/
  
  # specify login credentials for the climate data store
  user_id = "f64421c8-a4c9-4c16-9b25-d2914edc68dc" # this can be found on your user profile
  api_key = "6913841f-d568-40b4-9d22-11da042862f8" # this is the API key, also found on your profile
  
  # set key
  wf_set_key(user = user_id,
             key = api_key) 
  
  # specify request for current year
  request <- list(
    "dataset_short_name" = "reanalysis-era5-single-levels-monthly-means",
    "product_type" = "monthly_averaged_reanalysis",
    "variable" = c("sea_ice_cover"),    
    "year" = ice.years,                     
    "month" = sprintf("%02d", 1:12),
    "day" = sprintf("%02d", 1:31),
    "time" = sprintf("%02d:00", 0:23),
    "area" = c(64, -180, 55, -160),           # Bering 
    "format" = "netcdf",                  
    "target" = paste0("ERA5_ice_", min(ice.years), "-", current.year, ".nc") # target file name
  )
  
  # run request (you may need to manually click accept license on website --> follow link in error message if it appears)
  wf_request(
    user     = user_id,
    request  = request,
    transfer = TRUE,
    path     = "./Data/Ice data", # where do you want the data to be saved?
    verbose = TRUE
  )

# 3) PROCESS ICE FILES ----
  # Specify unique ice file names
  ice.file <- paste0("./Data/", current.year, "/ERA5_ice_1952-", current.year, ".nc")

  # Process ice data using tidync()
  tidync(ice.file) %>%
    hyper_filter(longitude = longitude >= -180 & longitude <= -165,
                 latitude = latitude >= 55 & latitude <= 63) %>%
    activate("siconc") %>%
    hyper_tibble() %>%
    mutate(year = lubridate::year(valid_time),
           month = lubridate::month(valid_time),
           latitude = as.numeric(as.character(latitude)),
           longitude = as.numeric(as.character(longitude))) %>%
    filter(month %in% c(1:4)) %>% #months != Jan-Apr
    group_by(year, month)  %>%
    reframe(value= mean(siconc)) -> ice.means
      
      
  # Filter years greater than 1952, scale, and compute Jan-Feb and Mar-Apr means
  ice.means %>%
    filter(year > 1952) %>%
    group_by(month) %>%
    mutate(value = scale(value),
           name = case_when((month %in% 1:2) ~ "Jan-Feb ice cover",
                            TRUE ~ "Mar-Apr ice cover")) %>%
    ungroup() %>%
    group_by(year, name) %>%
    reframe(value = mean(value)) %>%
    filter(year >= 1972) -> ice.dat
    

  # Save
  write.csv(ice.dat, paste0("./Output/", current.year, "ice.csv", row.names = FALSE))
  