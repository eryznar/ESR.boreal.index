# Bottom temp ----
old.bt <- read.csv("C:/Users/emily.ryznar/Work/Documents/Boreal opie/boreal-opieNEW/Data/date_corrected_bottom_temp.csv") %>%
          mutate(type = "old")

new.bt <- read.csv("./Output/date_corrected_bottom_temp.csv") %>%
        mutate(type = "new")

ggplot(rbind(old.bt, new.bt), aes(year, bottom.temp, color = type))+
  geom_line()+
  geom_point()


# Ice ----
old.ice <- read.csv("C:/Users/emily.ryznar/Work/Documents/Boreal opie/boreal-opieNEW/Data/ice.csv") %>%
  mutate(type = "old")

new.ice <- read.csv("./Output/ice.csv") %>%
  mutate(type = "new")

ggplot(rbind(old.ice, new.ice), aes(year, JanFeb_ice, color = type))+
  geom_line()+
  geom_point()

ggplot(rbind(old.ice, new.ice), aes(year, MarApr_ice, color = type))+
  geom_line()+
  geom_point()

# Bloom type and timing ----
old.bloomtime <- read.csv("C:/Users/emily.ryznar/Work/Documents/Boreal opie/boreal-opieNEW/Data/bloom_timing.csv") %>%
  filter(north_south == "south") %>%
  rename(value = globcolour_peak_mean) %>%
  mutate(name = "Bloom timing") %>%
  dplyr::select(year, name, value) %>%
  mutate(type = "old")

new.bloomtime <- read.csv("./Output/bloom_timing.csv") %>%
  mutate(type = "new") %>%
  dplyr::select(!X)

ggplot(rbind(old.bloomtime, new.bloomtime), aes(year, value, color = type))+
  geom_line()+
  geom_point()

old.bloomtype <- read.csv("C:/Users/emily.ryznar/Work/Documents/Boreal opie/boreal-opieNEW/Data/bloom_type.csv") %>%
  filter(north_south == "south") %>%
  mutate(name = case_when(gl_type == "ice_full" ~ "Ice-edge bloom",
                          gl_type == "ice_free" ~ "Open water bloom")) %>%
  rename(value = count) %>%
  dplyr::select(year, name, value)%>%
  filter(name == "Open water bloom") %>%
  mutate(type = "old")

new.bloomtype <- read.csv("./Output/bloom_type.csv") %>%
  mutate(type = "new") %>%
  dplyr::select(!X)

ggplot(rbind(old.bloomtype, new.bloomtype), aes(year, value, color = type))+
  geom_line()+
  geom_point()

# Groundfish biomass ----
old.gf <- read.csv("C:/Users/emily.ryznar/Work/Documents/Boreal opie/boreal-opieNEW/Data/groundfish_mean_cpue.csv") %>%
        mutate(type = "old")

new.gf <- read.csv("./Output/groundfish_mean_cpue.csv") %>%
  mutate(type = "new") 

ggplot(rbind(old.gf, new.gf), aes(YEAR, mean_cod_CPUE, color = type))+ # won't match bc diff core area and units
  geom_line()+
  geom_point()

ggplot(rbind(old.gf, new.gf), aes(YEAR, mean_arctic_CPUE, color = type))+ # won't match bc diff core area and units
  geom_line()+
  geom_point()

# Zooplankton ----
old.zoop <- read.csv("C:/Users/emily.ryznar/Work/Documents/Boreal opie/boreal-opieNEW/Data/summarized_zooplankton.csv") %>%
            mutate(type = "old")

new.zoop <- read.csv("./Output/summarized_zooplankton.csv") %>%
  mutate(type = "new") 

ggplot(rbind(old.zoop, new.zoop) %>% filter(taxa == "Pseudocalanus"), aes(year, log_abundance, color = type))+ 
  geom_line()+
  geom_point()

ggplot(rbind(old.zoop, new.zoop) %>% filter(taxa == "Calanus_glacialis"), aes(year, log_abundance, color = type))+ 
  geom_line()+
  geom_point()
