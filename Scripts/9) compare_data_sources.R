# Bottom temp ----
old.bt <- read.csv(paste0("./Output/", prev.year, "/date_corrected_bottom_temp.csv")) %>%
        mutate(type = "old", name = "Bottom temperature") %>%
        rename(value = "bottom.temp")

new.bt <- read.csv(paste0("./Output/", current.year, "/date_corrected_bottom_temp.csv")) %>%
  mutate(type = "new")

ggplot(rbind(old.bt, new.bt), aes(year, value, color = type))+
  geom_line()+
  geom_point()


# Ice ----
old.ice <- read.csv(paste0("./Output/", prev.year, "/ice.csv")) %>%
  mutate(type = "old")

new.ice <- read.csv(paste0("./Output/", current.year, "/ice.csv")) %>%
  mutate(type = "new")

ggplot(rbind(old.ice, new.ice) %>% filter(name == "Jan-Feb ice cover"), aes(year, value, color = type))+
  geom_line()+
  geom_point()

ggplot(rbind(old.ice, new.ice) %>% filter(name == "Mar-Apr ice cover"), aes(year, value, color = type))+
  geom_line()+
  geom_point()

# Bloom type and timing ----
old.bloomtime <- read.csv(paste0("./Output/", prev.year, "/bloom_timing.csv")) %>%
      mutate(type = "old")

new.bloomtime <- read.csv(paste0("./Output/", current.year, "/bloom_timing.csv")) %>%
  mutate(type = "new")

ggplot(rbind(old.bloomtime, new.bloomtime), aes(year, value, color = type))+
  geom_line()+
  geom_point()

old.bloomtype <- read.csv(paste0("./Output/", prev.year, "/bloom_type.csv")) %>%
  mutate(type = "old")

new.bloomtype <- read.csv(paste0("./Output/", current.year, "/bloom_type.csv")) %>%
  mutate(type = "new")

ggplot(rbind(old.bloomtype, new.bloomtype), aes(year, value, color = type))+
  geom_line()+
  geom_point()

# Groundfish biomass ----
old.gf <- read.csv(paste0("./Output/", prev.year, "/groundfish_mean_cpue.csv")) %>%
        mutate(type = "old") %>%
        rename('Pacific cod' = mean_cod_CPUE,
               'Arctic groundfish' = mean_arctic_CPUE,
               year = YEAR) %>%
        dplyr::select(!X) %>%
        pivot_longer(cols = 2:3)

new.gf <- read.csv(paste0("./Output/", current.year, "/groundfish_mean_cpue.csv")) %>%
  mutate(type = "new") %>%
  dplyr::select(!X)

ggplot(rbind(old.gf, new.gf) %>% filter(name == "Pacific cod"), aes(year, value, color = type))+ # won't match bc diff core area and units
  geom_line()+
  geom_point()

ggplot(rbind(old.gf, new.gf) %>% filter(name == "Arctic groundfish"), aes(year, value, color = type))+ # won't match bc diff core area and units
  geom_line()+
  geom_point()

# Zooplankton ----

