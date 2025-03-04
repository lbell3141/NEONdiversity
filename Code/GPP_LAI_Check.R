#Check GPP and LAI
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

#Load Data
FluxDataFolder <- "X:/moore/FluxNetData"
modisLAI <- "./Data/Annual_MODIS_LAI_Extraction.csv"
badmLAI <- "./Data/LAI_BADM.csv"
IGBPmetadata <- "./Data/AMF_AA-Net_BIF_CCBY4_20250201.csv"


IGBP_df <- read.csv(IGBPmetadata)%>%
  filter(VARIABLE == "IGBP")%>%
  select(-c(VARIABLE_GROUP, VARIABLE, GROUP_ID))%>%
  rename(SiteID = SITE_ID,
         IGBP_PI = DATAVALUE)
#Site GPP

SubFolderPaths <- list.dirs(FluxDataFolder, recursive = T, full.names = T)
annual_dat <- unlist(lapply(SubFolderPaths, function(x) {
  list.files(x, pattern = "FLUXNET_SUBSET_YY", full.names = T)
}))
dfs_wGPPzscores <- lapply(annual_dat, function(x) {
  read.csv(x, na.strings = "-9999", sep = ",")%>%
    select(TIMESTAMP, GPP_DT_VUT_75)%>%
    mutate(AnnualGPP = GPP_DT_VUT_75,
           GPPLongtermAvg = mean(GPP_DT_VUT_75, na.rm = T),
           GPPLongtermSD = sd(GPP_DT_VUT_75, na.rm = T),
           GPPZScore = (GPP_DT_VUT_75 - GPPLongtermAvg) / GPPLongtermSD)
})
#add site names and convert list to df
site_codes <- list.files(FluxDataFolder, full.names = T) %>%
  gsub(".*([A-Z]{2}-[A-Za-z0-9]{3}).*", "\\1", .)
dfs_wGPPzscores <- Map(function(df, site) {
  df %>%
    mutate(SiteID = site)
}, dfs_wGPPzscores, site_codes)

gpp_df <- bind_rows(dfs_wGPPzscores)%>%
  relocate(SiteID, TIMESTAMP)
  
###
#tower location csv
BADM <- read.csv("./Data/AMF_AA-Net_BIF_CCBY4_20250201.csv")

lat_lon_df <- BADM%>%
  filter(VARIABLE %in% c("LOCATION_LAT", "LOCATION_LONG"))%>%
  group_by(SITE_ID)%>%
  filter(GROUP_ID == max(GROUP_ID)) %>%
  ungroup()%>%
  select(-c(VARIABLE_GROUP, GROUP_ID))%>%
  pivot_wider(names_from = VARIABLE, values_from = DATAVALUE)

#write.csv(lat_lon_df, "./Data/TowerLocations.csv", row.names = F)

#
#load MODIS LAI; MCD15A3H.061
modisLAI_df <- read.csv(modisLAI)%>%
  select(SITE_ID, year, mean)%>%
  rename(Year = year,
         AverageLAI = mean,
         SiteID = SITE_ID)%>%
  arrange(SiteID)
gpp_voi <- gpp_df[,1:3]%>%
  rename(Year = TIMESTAMP)
combovar <- merge(gpp_voi, modisLAI_df, by = c("SiteID", "Year"))
rec_avg <- combovar%>%
  group_by(SiteID)%>%
  summarise(LongtermLAI = mean(AverageLAI, na.rm = T),
            LongtermGPP = mean(GPP_DT_VUT_75, na.rm = T))

vegclasslongterm <- merge(rec_avg, IGBP_df, by = "SiteID")

#plot relationship 
longtermGPPLAI <- ggplot(vegclasslongterm, aes(LongtermLAI, LongtermGPP))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, color = "maroon")+
  labs(x = "Average LAI", y = "Average GPP")+
  theme_minimal()
longtermGPPLAI

longtermGPPLAIigbp <- ggplot(vegclasslongterm, aes(LongtermLAI, LongtermGPP)) +
  geom_point() +
  labs(x = "LAI Longterm Average", 
       y = "GPP Longterm Average") +
  theme_minimal() +
  facet_wrap(~ IGBP_PI, scales = "free")
longtermGPPLAIigbp


annualGPPLAI <- ggplot(combovar, aes(AverageLAI, GPP_DT_VUT_75))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, color = "maroon")+
  theme_minimal()
annualGPPLAI

#BADM LAI and longterm GPP
badmLAIdf <- read.csv(badmLAI)%>%
  rename(SiteID = SITE_ID,
         SiteLAI = LAI_TOT_Mean)
ltgpp <- gpp_voi%>%
  group_by(SiteID)%>%
  summarise(longtermGPP = mean(GPP_DT_VUT_75, na.rm = T))
badmgpp <- merge(ltgpp, badmLAIdf, by = "SiteID")%>%
  select(-NumRegister_LAI)
vegclass <- merge(badmgpp, IGBP_df, by = "SiteID")

longtermGPPsiteLAI <- ggplot(badmgpp, aes(SiteLAI, longtermGPP))+
  geom_point()+
  geom_smooth(method = "lm", se = F, color = "maroon")+
  labs(x = "Average LAI", y = "Average GPP")+
  theme_minimal()
longtermGPPsiteLAI


GPPLAIigbp <- ggplot(vegclass, aes(SiteLAI, longtermGPP)) +
  geom_point() +
  labs(x = "LAI Longterm Average", 
       y = "GPP Longterm Average") +
  theme_minimal() +
  facet_wrap(~ IGBP_PI, scales = "free")
GPPLAIigbp