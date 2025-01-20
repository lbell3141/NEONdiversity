#Script to create a dataframe with standardized annual GPP and Biomass estimates
#as well as seasonal precipitaiton conditions 

#Sections of script are: 
# 1. Define Filepaths
# 2. Load Necessary Packages
# 3. Load Data and Select Columns of Interest
# 4. Determine Summer and Winter Rainy Seasons Dynamically Using Break Points
# 5. Calculate Seasonal Precip Z-Scores
# 6. Calculate Annual GPP Z-Scores
# 7. Calculate Annual Biomass Z-Scores
# 8. Finalize and Save Dataframe 

#Biomass data: https://github.com/EcosystemEcologyLab/fluxtower-sites.git
#Flux Data = Ameriflux FluxNet Daily Products

#===============================================================================
#Define Filepaths
#===============================================================================
FluxDataFolder <- "./Data/FluxData/FluxNetDaily"
BiomassData <- "./Data/Biomass/BiomassFullOutput.csv"
ScriptOutputPath <- "./Data/ScriptOutput/Output.csv"
#===============================================================================
#Load Necessary Packages 
#===============================================================================
library(lubridate)
library(dplyr)
library(pbapply)
library(BreakPoints)
library(tidyverse)
library(stringr)
#===============================================================================
#Load Data and Select Columns of Interest
#===============================================================================
#load in all flux data:
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
#===============================================================================
#Determine Summer and Winter Rainy Seasons Dynamically Using Break Points
#===============================================================================
#Compute breakpoints (output is annoyingly nested list)
NestedBrkPts_dfs <- pblapply(raw_data_WYrecord, function(i) {
  i %>%
    group_by(WaterYr) %>%
    summarize(
      BreakPoints = N_break_point(AccPrecip, n_max = 2, n_period = 15, seed_set = 4557)
    )})
#extract breakpoints from output list and store in new list of dfs
ExtractedBrkPts_dfs <- lapply(NestedBrkPts_dfs, function(i) {
  lapply(seq_along(i$BreakPoints), function(index) {
    j <- i$BreakPoints[[index]]
      BP1 <- j$breaks[[2]][1]
      BP2 <- j$breaks[[2]][2]
      WaterYr <- i$WaterYr[index]
    
    data.frame(WaterYr = WaterYr, BP1 = BP1, BP2 = BP2)
  })
})
#combine lists together for 1 df per site
BrkPts_bind <- lapply(ExtractedBrkPts_dfs, function(i){
  i%>%
    bind_rows()
})

#Add BPs back into dfs based on WaterYr and Site List #
merged_dfs <- mapply(function(i, j) {
  merge(i, j, by = "WaterYr")
}, raw_data_WYrecord, BrkPts_bind, SIMPLIFY = F)

# Use breakpoints to assign seasons
dfs_rainy_season <- lapply(merged_dfs, function(i){
  i%>%
    mutate(Season = case_when(
        WYrecord < BP1 ~ "Winter",
        WYrecord > BP2 ~ "Summer",
        TRUE ~ "Spring"))
    })

dfs_rainy_season <- lapply(dfs_rainy_season, function(i){
  i%>%
    filter(Season != "Spring")})
#===============================================================================
#Calculate Seasonal Precip Z-Scores
#===============================================================================
dfs_wLongterm_precip <- lapply(dfs_rainy_season, function(i){
  i%>%
    group_by(WaterYr, Season)%>%
      mutate(SeasonalPrecip = sum(P_F, na.rm = T))%>%
    ungroup()%>%
    group_by(Season)%>%
      mutate(PrecipLongtermAvg = mean(SeasonalPrecip, na.rm = T),
             PrecipLongtermSD = sd(SeasonalPrecip, na.rm = T))
})

precip_zscore_dfs <- lapply(dfs_wLongterm_precip, function(i){
  i%>%
    group_by(WaterYr, Season)%>%
    mutate(PrecipZScore = (SeasonalPrecip - PrecipLongtermAvg)/PrecipLongtermSD)
})

#===============================================================================
#Calculate Annual GPP Z-Scores
#===============================================================================
dfs_wLongterm_gpp <- lapply(precip_zscore_dfs, function(i){
  i%>%
    group_by(WaterYr)%>%
    mutate(AnnualGPP = sum(GPP_DT_VUT_75 * 86400, na.rm = T))%>%
    ungroup()%>%
    mutate(GPPLongtermAvg = mean(AnnualGPP, na.rm = T),
           GPPLongtermSD = sd(AnnualGPP, na.rm = T))
})

dfs_wGPPzscores <- lapply(dfs_wLongterm_gpp, function(i){
  i%>%
    group_by(WaterYr)%>%
    mutate(GPPZScore = (AnnualGPP - GPPLongtermAvg)/GPPLongtermSD)
})

#===============================================================================
#Calculate Annual Biomass Z-Scores
#===============================================================================


#===============================================================================
#Finalize and Save Dataframe
#===============================================================================
distinct_list <- lapply(dfs_wGPPzscores, function(i) {
  i%>%
    select(WaterYr, Season, PrecipZScore, GPPZScore)%>%
    distinct()})

list_w_relevant_columns <- lapply(distinct_list, function(i) {
  i%>%
    pivot_wider(names_from = Season, 
                values_from = PrecipZScore) %>%
    select(WaterYr, Winter, Summer, GPPZScore)
})

#add site codes 
site_codes <- gsub(".*(US-[A-Za-z]+).*", "\\1", raw_data_list)
list_w_relevant_columns <- Map(function(df, site) {
  df %>%
    mutate(SiteID = site)
}, list_w_relevant_columns, site_codes)

#collapse list into single df and clean up format
results_df <- bind_rows(list_w_relevant_columns)
final_df <- results_df%>%
  rename(SummerPrecipZScore = Summer, 
         WinterPrecipZScore = Winter,
         WaterYear = WaterYr)
final_df <- final_df%>%
  mutate(PrecipCondition = case_when(
    WinterPrecipZScore < 0 & SummerPrecipZScore < 0 ~ "DD",
    WinterPrecipZScore < 0 & SummerPrecipZScore > 0 ~ "DW",
    WinterPrecipZScore > 0 & SummerPrecipZScore < 0 ~ "WD",
    WinterPrecipZScore > 0 & SummerPrecipZScore > 0 ~ "WW"))%>%
  select(SiteID, WaterYear, PrecipCondition, WinterPrecipZScore, SummerPrecipZScore, GPPZScore)

#save df
write.csv(final_df, file = ScriptOutputPath, row.names = F)
