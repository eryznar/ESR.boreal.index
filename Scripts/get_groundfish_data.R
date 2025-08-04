# Load libs/params
source("./Scripts/load_libs_params.R")

PKG <- c("RODBC")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

channel<-odbcConnect(dsn = "AFSC",
                     uid = "FILL IN", 
                     pwd = "FILL IN", 
                     believeNRows = FALSE)

odbcGetInfo(channel)


#Or can also use with manual entry: 
channel = gapindex::get_connected()


# Manipulate with SQL using AKFIN data before bringing into R ------------------
# This will pull a final, completely formatted table for only EBS and NBS
#FYI: This is a long run time, and you must be connected to VPN!

a <- RODBC::sqlQuery(channel = channel, # NOT RACEBASE.HAUL
                     query = paste0(
                       "SELECT
cr.CRUISEJOIN,
cr.CRUISE,
cr.YEAR,
cr.SURVEY_DEFINITION_ID,
cr.SURVEY_NAME,
cr.VESSEL_ID,
cr.VESSEL_NAME,
cp.HAULJOIN,
cp.SPECIES_CODE,
tt.SPECIES_NAME,
tt.COMMON_NAME,
cp.WEIGHT_KG,
cp.COUNT,
cp.AREA_SWEPT_KM2,
cp.CPUE_KGKM2,
cp.CPUE_NOKM2,
-- cp.CPUE_KGKM2/100 AS WTCPUE,
-- cp.CPUE_NOKM2/100 AS NUMCPUE,
hh.HAUL,
hh.STATION,
hh.LATITUDE_DD_START,
hh.LATITUDE_DD_END,
hh.LONGITUDE_DD_START,
hh.LONGITUDE_DD_END
FROM GAP_PRODUCTS.AKFIN_HAUL hh
LEFT JOIN GAP_PRODUCTS.AKFIN_CRUISE cr
ON hh.CRUISEJOIN = cr.CRUISEJOIN
LEFT JOIN GAP_PRODUCTS.AKFIN_CPUE cp
ON hh.HAULJOIN = cp.HAULJOIN
LEFT JOIN GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION tt
ON cp.SPECIES_CODE = tt.SPECIES_CODE
WHERE SURVEY_DEFINITION_ID IN (143, 98) -- 143 NBS, 98 EBS
AND tt.SURVEY_SPECIES = 1;")) 

write.csv(x = a, 
          here::here("Data","gf_cpue_timeseries.csv"))