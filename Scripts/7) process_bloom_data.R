# NEW DATA (<2024 uses globcolour, >=2024 uses occci) ---
#bloom timing
d3 <- read.csv("./Data/2024/bloom_timing_NS_OPIE_2024.csv")
bloomtime <- read.csv(paste0("./Data/", current.year, "/bloom_timing_2024.csv"))



d3 <- d3 %>%
  filter(north_south == "south") %>%
  rename(value = mean_peak) %>%
  mutate(name = "Bloom timing") %>%
  dplyr::select(year, name, value)

bloomtime <- bloomtime %>%
  filter(north_south == "south") %>%
  rename(value = mean_peak) %>%
  mutate(name = "Bloom timing") %>%
  dplyr::select(year, name, value)



#bloom type  
d4 <- read.csv("./Data/2024/bloom_type_ESP_crab_middle_outer_with2024.csv")
bloomtype <- read.csv(paste0("./Data/", current.year, "/bloom_type_2024.csv"))

check  <- bloomtype %>%
  filter(north_south == "south") %>%
  rename("Open water bloom" = "ice_free", "Ice-edge bloom" = "ice_full") %>%
  pivot_longer(!c(1:2, 5)) %>%
  dplyr::select(year, name, value) %>%
  group_by(name) %>%
  summarise(count = n())

check

d4  <- d4 %>%
  filter(north_south == "south") %>%
  rename("Open water bloom" = "ice_free", "Ice-edge bloom" = "ice_full") %>%
  pivot_longer(!c(1:2, 5)) %>%
  dplyr::select(year, name, value)%>%
  filter(name == "Open water bloom")

bloomtype <- bloomtype %>%
  filter(north_south == "south") %>%
  rename("Open water bloom" = "ice_free", "Ice-edge bloom" = "ice_full") %>%
  pivot_longer(!c(1:2, 5)) %>%
  dplyr::select(year, name, value)%>%
  filter(name == "Open water bloom") %>%
  mutate(value = case_when((year == 2025) ~  NA,
            TRUE ~ value))

# COMPARE ---
ggplot(rbind(d3 %>% mutate(type = "Old"), bloomtime %>% mutate(type = "New")), aes(year, value, color = type))+
  geom_line()+
  geom_point()

ggplot(rbind(d4 %>% mutate(type = "Old"), bloomtype %>% mutate(type = "New")), aes(year, value, color = type))+
  geom_line()+
  geom_point() +
  ggtitle("open water bloom = 'ice_free', S region")

write.csv(bloomtime, paste0("./Output/", current.year, "/bloom_timing.csv"))
write.csv(bloomtype, paste0("./Output/", current.year, "/bloom_type.csv"))
