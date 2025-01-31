#Calculate Aridity Index

library(SPEI)

FluxDataFolder <- "X:/moore/FluxNetData"

SubFolderPaths <- list.dirs(FluxDataFolder, recursive = T, full.names = T)
daily_dat <- unlist(lapply(SubFolderPaths, function(x) {
  list.files(x, pattern = "FLUXNET_SUBSET_DD", full.names = T)
}))
monthly_inputs <- lapply(daily_dat, function(x) {
  read.csv(x, na.strings = "-9999", sep = ",")%>%
    select(TIMESTAMP, TA_F, SW_IN_F, P_F)%>%
    mutate(TIMESTAMP = ymd(TIMESTAMP),
           Month = month(TIMESTAMP))%>%
    group_by(Month)%>%
    summarize(Tmax = max(TA_F, na.rm = T),
              Tmin = min(TA_F, na.rm = T),
              Ra = mean(SW_IN_F, na.rm = T)*0.0036,
              P = mean(P_F, na.rm = T))
  })
monthly_PET <- lapply(monthly_inputs, function(i){
  i%>%
    summarise(PET = hargreaves(Tmin = Tmin, Tmax = Tmax, Ra = Ra),
              P = P)
})
site_codes <- list.files(FluxDataFolder, full.names = T) %>%
  gsub(".*([A-Z]{2}-[A-Za-z0-9]{3}).*", "\\1", .)
sitePET_list <- Map(function(df, site) {
  df %>%
    mutate(SiteID = site)
}, monthly_PET, site_codes)
siteAI <- bind_rows(sitePET_list)%>%
  group_by(SiteID)%>%
  summarize(PET = mean(PET, na.rm = T),
            P = mean(P, na.rm = T))%>%
  mutate(AI = P/PET)

LongDatwAI <- merge(AllProdLongtermDat, siteAI, by = "SiteID")
