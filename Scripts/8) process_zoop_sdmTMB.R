# PURPOSE: To process zooplankton data from sdmTMB indices (provided by David Kimmel)

# AUTHOR: Emily Ryznar

# 1) LOAD LIBS/PARAMS ----
source("./Scripts/1) load_libs_params.R")

# 2) LOAD, PROCESS, and SAVE DATA ----
copepods <- read.csv(paste0("./Data/", current.year, "/BS_summer_index.csv")) %>%
                  mutate(name = case_when((Taxa == "Copepods > 2 mm") ~ "large copepods",
                                          TRUE ~ "small copepods"),
                        log_abundance = log(est+10)) %>%
            dplyr::select(YEAR, name, log_abundance) %>%
            rename(year = YEAR)

write.csv(copepods, paste0("./Output/", current.year, "/sdmTMB_copepods.csv")
