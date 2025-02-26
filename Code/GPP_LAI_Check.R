#Check GPP and LAI
library(dplyr)
library(lubridate)
#Site GPP

FluxDataFolder <- "X:/moore/FluxNetData"
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
  

