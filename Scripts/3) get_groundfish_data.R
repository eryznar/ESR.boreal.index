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



write.csv(x = a, 
          here::here(paste0("Data/", current.year, "/gf_cpue_timeseries.csv")))

gapindex::upload_oracle(x = data.frame(A = 1),
                        table_name = "test_table",
                        metadata_column = data.frame(colname = "column A",
                                                     colname_long = "column A",
                                                     units = "NA",
                                                     datatype = "NUMBER(10,0)",
                                                     colname_desc = "value"),
                        table_metadata = "this is a test table",
                        channel = channel, ## replace with your channel object
                        schema = "ERYZNAR" ## replace with your oracle username
)
