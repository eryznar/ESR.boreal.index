# NEW DATA ---
#bloom timing
d3 <- read.csv("./Data/bloom_timing_NS_OPIE_2024.csv")

d3 <- d3 %>%
  filter(north_south == "south") %>%
  rename(value = mean_peak) %>%
  mutate(name = "Bloom timing") %>%
  dplyr::select(year, name, value)

#bloom type  
d4 <- read.csv("./Data/bloom_type_ESP_crab_middle_outer_with2024.csv")

check  <- d4 %>%
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

# COMPARE ---
ggplot(rbind(d1 %>% mutate(type = "Old"), d3 %>% mutate(type = "New")), aes(year, value, color = type))+
  geom_line()+
  geom_point()

ggplot(rbind(d2 %>% mutate(type = "Old"), d4 %>% mutate(type = "New")), aes(year, value, color = type))+
  geom_line()+
  geom_point()

write.csv(d3, "./Output/bloom_timing.csv")
write.csv(d4, "./Output/bloom_type.csv")
