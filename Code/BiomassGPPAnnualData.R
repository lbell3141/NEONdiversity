#create a dataframe with normalized tower GPP and biomass product MgC

#===============================================================================
#Load Necessary Packages
#===============================================================================
library(dplyr)
library(lubridate)
library(ggplot2)
#===============================================================================
#Define Filepaths
#===============================================================================
BiomassData <- "./Data/AnnualBiomassProducts.csv"
FluxDataFolder <- "X:/moore/FluxData"
IGBPmetadata <- "./Data/AmfIGBPmetadata.csv"
#ScriptOutputPath <- "./Data/ScriptOutput/NormalizedBioGPP.csv"
#===============================================================================
#Calculate Biomass Zscores by Site
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
#Calculate Flux Zscores by Site
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
#Combine Biomass, Flux, and IGBP Dataframes
#===============================================================================
IGBP_df <- read.csv(IGBPmetadata)%>%
  select(SITEID, IGBP_name)%>%
  rename(SiteID = SITEID)

gppbio <- merge(BioZscoreDf, FluxZscoreDf, by = c("SiteID", "Year"))
gppbio <- merge(gppbio, IGBP_df, by = "SiteID")

#also make longterm average frame
LTbio <- BioZscoreDfcalcs%>%
  select(Product, SiteID, BioLongtermAvg)%>%
  distinct()
LTGPP <- results_df%>%
  select(SiteID, GPPLongtermAvg)%>%
  distinct()
LongTermDf <- merge(LTbio, LTGPP, by = "SiteID")
LongTermDf <- merge(LongTermDf, IGBP_df, by = "SiteID")
#===============================================================================
#Visualize Data
#===============================================================================
AllProdAllSites <- ggplot(gppbio, aes(x = BiomassZscore, y = GPPZScore, color = Product)) +
  geom_point() +
  labs(x = "Biomass Z-Score", 
       y = "GPP Z-Score", 
       title = "Biomass-GPP Comparison",
       color = "Product") +
  theme_minimal()

AllSitesByProd <- ggplot(gppbio, aes(x = BiomassZscore, y = GPPZScore, color = IGBP_name)) +
  geom_point() +
  labs(x = "Biomass Z-Score", 
       y = "GPP Z-Score", 
       title = "Biomass-GPP Comparison",
       color = "IGBP_name") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")

Longterm_plot <- ggplot(LongTermDf, aes(x = BioLongtermAvg, y = GPPLongtermAvg, color = IGBP_name)) +
  geom_point() +
  labs(x = "Biomass Record Average", 
       y = "GPP Record Average", 
       title = "Biomass-GPP Longterm Average Comparison",
       color = "IGBP_name") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
