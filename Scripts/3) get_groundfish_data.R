# Load libs/params
source("./Scripts/1) load_libs_params.R")


#Or can also use with manual entry: 
channel <- gapindex::get_connected()

uid = "ERYZNAR" 
pwd = "daFFodilViolet100#"


gf_data<- gapindex::get_data(
            year_set = c(1975:2019, 2021:current.year),
            survey_set = c("EBS", "NBS"),
            spp_codes = c(21720, 10285,21725,10140,21348,10115,66045,24185,10212), #cod (first #), and arctic complex  
            haul_type = 3,
            abundance_haul = "Y",
            pull_lengths = F,
            channel = channel)


gf_cpue <- calc_cpue(gf_data)

haul <- gf_data$haul %>%
            dplyr::select(CRUISEJOIN, HAULJOIN, STATIONID)

gf_cpue <- right_join(gf_cpue, haul)


write.csv(gf_cpue, paste0("Data/", current.year, "/gf_cpue_timeseries.csv"))

