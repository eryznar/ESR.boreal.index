# Notes:
# 1) Do we use only summer copepod data?
# 2) Are sdmTMB indices date and location corrected?
# 3) Do you want small and large copepods separated or combined?


copepods <- rbind(read.csv(paste0("./Data/", prev.year, "/Copepods_GT2_summer_index.csv")) %>%
                  mutate(name = "small copepods"),# small copepods
                  read.csv(paste0("./Data/", prev.year, "/Copepods_LT2_summer_index.csv")) %>%
                  mutate(name = "large copepods")) %>%
            mutate(log_abundance = log(est+10)) %>%
            dplyr::select(YEAR, name, log_abundance) %>%
            rename(year = YEAR)

write.csv(copepods, paste0("./Output/", prev.year, "/sdmTMB_copepods.csv"))
