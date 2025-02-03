library(zip)
library(readr)
library(lubridate)
library(dplyr)
library(stringr)

#extract flux data from zip files and store in R obj
folder_path <- "X:/moore/ICOSdata"
zip_files <- list.files(folder_path, pattern = "FLUXNET_HH_L2\\.zip$", full.names = TRUE)
read_fluxnet_csv <- function(zip_file) {
  files_in_zip <- zip::zip_list(zip_file)$filename
  csv_file <- files_in_zip[grepl("\\.csv$", files_in_zip)]
  if (length(csv_file) == 1) {
    con <- unz(zip_file, csv_file)
    df <- read_csv(con)
    return(df)
  } else {
    warning(paste("No CSV found in:", zip_file))
    return(NULL)
  }
}
fluxnet_data <- lapply(zip_files, read_fluxnet_csv)
extract_site_code <- function(filename) {
  str_extract(filename, "[A-Z]{2}-[A-Za-z]{3}")
}
fluxnet_data <- lapply(names(fluxnet_data), function(name) {
  df <- fluxnet_data[[name]]
  site_code <- extract_site_code(name)
  
  if (!is.null(df)) {
    df <- df %>% mutate(Site_Code = site_code)
  }
  
  return(df)
})
names(fluxnet_data) <- basename(zip_files)
#saveRDS(fluxnet_data, "./Data/ICOSlist.RDS")
flux_coi <- lapply(fluxnet_data, function(i){
  i %>%
    select(TIMESTAMP_START, GPP_DT_VUT_75) %>%
    mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START),
           Year = year(TIMESTAMP_START),
           DOY = yday(TIMESTAMP_START))%>%
    group_by(Year, DOY)%>%
    mutate(dailyGPP = mean(GPP_DT_VUT_75, na.rm = T))%>%
    ungroup()%>%
    group_by(Year)%>%
    summarize(TotalGPP = sum(GPP_DT_VUT_75, na.rm = T)) %>%
    mutate(GPPLongtermAvg = mean(TotalGPP, na.rm = T),
           GPPLongtermSD = sd(TotalGPP, na.rm = T),
           GPPZScore = (TotalGPP - GPPLongtermAvg) / GPPLongtermSD)
})

removal_site <-  c("DE-BeR", "FI-Kmp", "FI-Kur", "FI-Tum", "GR-HeK", "GR-Hem", "IT-BFt")
#Add ICOS data to FluxNet data
#===============================================================================
#extract IGBP from SiteInfo in Archive folders
zip_files <- list.files(folder_path, pattern = "ARCHIVE_L2\\.zip$", full.names = TRUE)
read_archive_csv <- function(zip_file) {
  files_in_zip <- zip::zip_list(zip_file)$filename
  csv_file <- files_in_zip[grepl("SITEINFO_L2.csv$", files_in_zip)]
  if (length(csv_file) == 1) {
    con <- unz(zip_file, csv_file)
    df <- read_csv(con)
    return(df)
  } else {
    warning(paste("No CSV found in:", zip_file))
    return(NULL)
  }
}
archive_data <- lapply(zip_files, read_archive_csv)
names(archive_data) <- basename(zip_files)

arch_df <- bind_rows(archive_data)%>%
  filter(VARIABLE == "IGBP")%>%
  select(SITE_ID, DATAVALUE)%>%
  rename(SiteID = SITE_ID,
         IGBP_PI = DATAVALUE)
