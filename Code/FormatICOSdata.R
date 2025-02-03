#extract flux data from zip files and store in R obj
#===============================================================================
# Load Necessary Packages
#===============================================================================
library(zip)
library(readr)
library(lubridate)
library(dplyr)
library(stringr)
#===============================================================================
# Extract Half-Hourly Data from Zip Folders on Snow X Drive
#===============================================================================
ICOSdatapath <- "X:/moore/ICOSdata"

zip_files <- list.files(ICOSdatapath, pattern = "FLUXNET_HH_L2\\.zip$", full.names = T)
fluxnet_data <- lapply(zip_files, function(i) {
  files_in_zip <- zip::zip_list(i)$filename
  csv_file <- files_in_zip[grepl("\\.csv$", files_in_zip)]
  
  con <- unz(i, csv_file)
  df <- read_csv(con)
  return(df)
})

#extract site code and add to dfs
extract_site_code <- function(filename) {
  str_extract(filename, "[A-Z]{2}-[A-Za-z]{3}")
}
fluxnet_data <- setNames(fluxnet_data, basename(zip_files))
#sites without HH FluxNet product
removal_site <- c("DE-BeR", "FI-Kmp", "FI-Kur", "FI-Tum", "GR-HeK", "GR-Hem", "IT-BFt")
fluxnet_data <- lapply(names(fluxnet_data), function(name) {
  df <- fluxnet_data[[name]]
  site_code <- extract_site_code(name)
  
  if (!is.null(df) && !site_code %in% removal_site) {
    df <- df %>% mutate(Site_Code = site_code)
    return(df)
  } else {
    return(NULL)
  }
})

#saveRDS(fluxnet_data, "./Data/ICOSlist.RDS")
#format like Ameriflux to combine dfs
flux_coi <- lapply(fluxnet_data, function(i){
  i %>%
    select(Site_Code, TIMESTAMP_START, GPP_DT_VUT_75) %>%
    mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START),
           Year = year(TIMESTAMP_START),
           DOY = yday(TIMESTAMP_START))%>%
    group_by(Year, DOY)%>%
    mutate(dailyGPP = mean(GPP_DT_VUT_75, na.rm = T))%>%
    ungroup()%>%
    group_by(Year)%>%
    summarize(TotalGPP = sum(GPP_DT_VUT_75, na.rm = T),
              Site_Code = Site_Code) %>%
    ungroup()%>%
    distinct()%>%
    mutate(GPPLongtermAvg = mean(TotalGPP, na.rm = T),
           GPPLongtermSD = sd(TotalGPP, na.rm = T),
           GPPZScore = (TotalGPP - GPPLongtermAvg) / GPPLongtermSD)%>%
    rename(SiteID = Site_Code)
})

fluxdf <- bind_rows(flux_coi)
#===============================================================================
#Pull IGBP from Archive folders
#===============================================================================
#extract IGBP from SiteInfo in Archive folders
zip_files <- list.files(folder_path, pattern = "ARCHIVE_L2\\.zip$", full.names = TRUE)
extract_site_code <- function(filename) {
  str_extract(filename, "[A-Z]{2}-[A-Za-z]{3}")
}
removal_site <- c("DE-BeR", "FI-Kmp", "FI-Kur", "FI-Tum", "GR-HeK", "GR-Hem", "IT-BFt")
zip_files <- zip_files[!extract_site_code(basename(zip_files)) %in% removal_site]
archive_data <- lapply(zip_files, function(i) {
  files_in_zip <- zip_list(i)$filename
  csv_file <- files_in_zip[grepl("SITEINFO_L2.csv$", files_in_zip)]
  con <- unz(i, csv_file)
  df <- read_csv(con)
    return(df)
  })
names(archive_data) <- basename(zip_files)

arch_df <- bind_rows(archive_data)%>%
  filter(VARIABLE == "IGBP")%>%
  select(SITE_ID, DATAVALUE)%>%
  rename(SiteID = SITE_ID,
         IGBP_PI = DATAVALUE)

ICOSdf <- merge(fluxdf, arch_df, by = "SiteID")

ggplot(ICOSdf, aes(x = Year, y = GPPZScore, color = IGBP_PI))+
  geom_point()+
  theme_minimal()
