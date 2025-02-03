#This script calculates annual zscores for biomass products with multi-year records
#It also determines the record average and matches to flux records

#The script is broken into the following sections:
  #1. Load Necessary Packages
  #2. Define Filepaths
  #3. Calculate Biomass Annual Zscores by Site
  #4. Calculate Flux Annual Zscores by Site
  #5. Combine Biomass, Flux, and IGBP Dataframes
  #6. Calculate Record Average (Flux/Biomass Temporally Aligned

#===============================================================================
#1. Load Necessary Packages
#===============================================================================
library(dplyr)
library(lubridate)
#===============================================================================
#2. Define Filepaths
#===============================================================================
BiomassData <- "./Data/AnnualBiomassProducts.csv"
FluxDataFolder <- "X:/moore/FluxNetData"
IGBPmetadata <- "./Data/AMF_AA-Net_BIF_CCBY4_20250201.csv"
#ScriptOutputPath <- "./Data/ScriptOutput/NormalizedBioGPP.csv"
#===============================================================================
#3. Calculate Biomass Annual Zscores by Site
#===============================================================================
BioZscoreDfcalcs <- read.csv(BiomassData)%>%
  select(-ffp_radius) %>%
  filter(!is.na(agb_Mg_ha),
         agb_Mg_ha > 0,
         !product %in% c("gedi", "menlove"), #have biomass for a date range instead of annual
         !product %in% c("gfw", "hfbs", "icesat", "nbcd"))%>% #have biomass for a single year, so can't calc zscores
  rename(Product = product,
         Year = year, 
         SiteID = SITE_ID,
         Biomass = agb_Mg_ha) %>%
  group_by(Product, SiteID) %>%
  mutate(BioLongtermAvg = mean(Biomass, na.rm = T),
         BioLongtermSD = sd(Biomass, na.rm = T),
         BiomassZscore = (Biomass - BioLongtermAvg) / BioLongtermSD)
BioZscoreDf <- BioZscoreDfcalcs%>%
  select(Product, Year, SiteID, BiomassZscore)
#===============================================================================
#4. Calculate Flux Annual Zscores by Site
#===============================================================================
SubFolderPaths <- list.dirs(FluxDataFolder, recursive = T, full.names = T)
daily_dat <- unlist(lapply(SubFolderPaths, function(x) {
  list.files(x, pattern = "FLUXNET_SUBSET_DD", full.names = T)
}))
dfs_wGPPzscores <- lapply(daily_dat, function(x) {
  read.csv(x, na.strings = "-9999", sep = ",")%>%
    select(TIMESTAMP, GPP_DT_VUT_75)%>%
    mutate(TIMESTAMP = ymd(TIMESTAMP),
           Year = year(TIMESTAMP))%>%
    group_by(Year)%>%
    summarize(TotalGPP = sum(GPP_DT_VUT_75, na.rm = T)) %>%
    mutate(GPPLongtermAvg = mean(TotalGPP, na.rm = T),
           GPPLongtermSD = sd(TotalGPP, na.rm = T),
           GPPZScore = (TotalGPP - GPPLongtermAvg) / GPPLongtermSD)
  })
#add site names and convert list to df
site_codes <- list.files(FluxDataFolder, full.names = T) %>%
  gsub(".*([A-Z]{2}-[A-Za-z0-9]{3}).*", "\\1", .)
dfs_wGPPzscores <- Map(function(df, site) {
  df %>%
    mutate(SiteID = site)
}, dfs_wGPPzscores, site_codes)

results_df <- bind_rows(dfs_wGPPzscores)
FluxZscoreDf <- results_df%>%
  select(SiteID, Year, GPPZScore)
#===============================================================================
#5. Combine Biomass, Flux, and IGBP Dataframes
#===============================================================================
IGBP_df <- read.csv(IGBPmetadata)%>%
  filter(VARIABLE == "IGBP")%>%
  select(-c(VARIABLE_GROUP, VARIABLE, GROUP_ID))%>%
  rename(SiteID = SITE_ID,
         IGBP_PI = DATAVALUE)

gppbio <- merge(BioZscoreDf, FluxZscoreDf, by = c("SiteID", "Year"))
gppbio <- merge(gppbio, IGBP_df, by = "SiteID")
#===============================================================================
#5. Calculate Record Average (Flux/Biomass Temporally Aligned
#===============================================================================
biodat <- read.csv(BiomassData)%>%
  select(-ffp_radius) %>%
  filter(!is.na(agb_Mg_ha))%>%
  rename(Product = product,
         Year = year, 
         SiteID = SITE_ID,
         Biomass = agb_Mg_ha)
#create separate dfs to average biomass products with a date range (instead of annual estimates)
GediMenSub <- biodat%>%
  filter(Product %in% c("menlove", "gedi"))%>%
  select(-Year)
fluxrangeGedi <- results_df%>%
  filter(Year %in% (2019:2023),
         TotalGPP != 0)%>%
  group_by(SiteID)%>%
  mutate(GPPLongtermAvg = mean(TotalGPP, na.rm = T),
         Product = "gedi")%>%
  select(SiteID, Product, GPPLongtermAvg)%>%
  distinct()
fluxrangeMenlove <- results_df%>%
  filter(Year %in% (2009:2019),
         TotalGPP != 0)%>%
  group_by(SiteID)%>%
  mutate(GPPLongtermAvg = mean(TotalGPP, na.rm = T),
         Product = "menlove")%>%
  select(SiteID, Product, GPPLongtermAvg)%>%
  distinct()  
DateRangeFlux <- rbind(fluxrangeGedi,fluxrangeMenlove)  
DateRangeFluxBio <- merge(DateRangeFlux, GediMenSub, by = c("SiteID", "Product"))%>%
  rename(BioLongtermAvg = Biomass)
#calc longterm avg for products with annual values and add to DateRangeFluxBio df
BF_dfa <- merge(biodat, results_df, by = c("Year", "SiteID"))%>%
  filter(TotalGPP != 0)%>%
  select(Year, SiteID, Product, TotalGPP, Biomass)
BF_dfb <- BF_dfa%>%
  group_by(SiteID, Product)%>%
  summarize(GPPLongtermAvg = mean(TotalGPP, na.rm = T),
            BioLongtermAvg = mean(Biomass, na.rm = T))

AllProdLongtermDat <- rbind(BF_dfb, DateRangeFluxBio)
#add IGBP classification 
IGBP_df <- read.csv(IGBPmetadata)%>%
  filter(VARIABLE == "IGBP")%>%
  select(-c(VARIABLE_GROUP, VARIABLE, GROUP_ID))%>%
  rename(SiteID = SITE_ID,
         IGBP_PI = DATAVALUE)
AllProdLongtermDat <- merge(AllProdLongtermDat, IGBP_df, by = "SiteID")%>%
  filter(BioLongtermAvg != 0)
