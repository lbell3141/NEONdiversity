library(dplyr)

BiomassData <- "./Data/AnnualBiomassProducts.csv"
BioDat <- read.csv(BiomassData)
BioDatIn <- BioDat%>%
  select(-ffp_radius)%>%
  filter(!is.na(agb_Mg_ha))%>%
  filter(agb_Mg_ha > 0)
testsites <- BioDatIn%>%
  filter(SITE_ID == c("US-SRM", "US-Whs", "US-Wkg"))
head(testsites)

FluxDataFolder <- "./Data/FluxData/DummyData"
raw_data_list <- list.files(FluxDataFolder, full.names = T)
raw_data_csvs <- lapply(raw_data_list, function(i) {
  read.csv(i, na.strings = "-9999", skip = 0, sep = ",")
})
#select columns of interest from flux data (time, precip, and GPP):
raw_data_coi <- lapply(raw_data_csvs, function(i){
  i%>%
    select(TIMESTAMP, P_F, GPP_DT_VUT_75)%>%
    mutate(TIMESTAMP = ymd(TIMESTAMP))%>%
    mutate(Year = year(TIMESTAMP),
           Month = month(TIMESTAMP),
           Day = day(TIMESTAMP),
           DOY = yday(TIMESTAMP))
})
#add water year: 
raw_data_wateryr <- lapply(raw_data_coi, function(i){
  i %>%
    mutate(WaterYr = ifelse(Month > 9, Year + 1, Year))
})
raw_data_WYrecord <- lapply(raw_data_wateryr, function(i){
  i %>%
    group_by(WaterYr)%>%
    mutate(WYrecord = row_number(),
           AccPrecip = cumsum(P_F))%>%
    filter(n() >= 330)%>% #partial water years causing issue with break points function, so remove
    ungroup()
})
