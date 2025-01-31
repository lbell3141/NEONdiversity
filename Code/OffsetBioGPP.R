#amending BiomassGPPAnnualData.R to offest products with annual estimates
#to GPP by one year to check for decoupling effect
#DOI: 10.1126/science.abm4875

#===============================================================================
#1. Load Necessary Packages
#===============================================================================
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpmisc)
#===============================================================================
#2. Define Filepaths
#===============================================================================
BiomassData <- "./Data/AnnualBiomassProducts.csv"
FluxDataFolder <- "X:/moore/FluxNetData"
IGBPmetadata <- "./Data/AmfIGBPmetadata.csv"
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
#offset bio by 1 year
BioZscoreDf$Year <- as.numeric(BioZscoreDf$Year)
OffsetBio <- BioZscoreDf%>%
  mutate(Year = Year - 1)


IGBP_df <- read.csv(IGBPmetadata)%>%
  select(SITEID, IGBP_name)%>%
  rename(SiteID = SITEID)

offsetgppbio <- merge(OffsetBio, FluxZscoreDf, by = c("SiteID", "Year"))
offsetgppbio <- merge(gppbio, IGBP_df, by = "SiteID")%>%
  distinct()
#===============================================================================
#5. Visualize Data
#===============================================================================
offtest <- offsetgppbio%>%
  filter(SiteID == "US-SRG")
Longterm_plot <- ggplot(offtest, aes(x = GPPZScore, y = BiomassZscore)) +
  geom_point() +
  labs(x = "Annual GPP Z Score", 
       y = "Annual Biomass Z Score", 
       title = "Offset-Biomass/GPP Comparison") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot

regtest <- gppbio%>%
  filter(SiteID=="US-SRG")
Longterm_plot <- ggplot(regtest, aes(x = GPPZScore, y = BiomassZscore)) +
  geom_point() +
  labs(x = "Annual GPP Z Score", 
       y = "Annual Biomass Z Score", 
       title = "Biomass/GPP Comparison") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot

#no site filter

#offset
Longterm_plot <- ggplot(offsetgppbio, aes(x = GPPZScore, y = BiomassZscore)) +
  geom_point() +
  geom_smooth(method = "lm", color = "maroon", se = F) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               formula = y ~ x, 
               parse = T) +
  labs(x = "Annual GPP Z Score", 
       y = "Annual Biomass Z Score", 
       title = "Offset-Biomass/GPP Comparison") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot

#regular (not offset)
Longterm_plot <- ggplot(gppbio, aes(x = GPPZScore, y = BiomassZscore)) +
  geom_point() +
  geom_smooth(method = "lm", color = "maroon", se = F) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               formula = y ~ x, 
               parse = T) +
  labs(x = "Annual GPP Z Score", 
       y = "Annual Biomass Z Score", 
       title = "Biomass/GPP Comparison") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot