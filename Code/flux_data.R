#flux anomalies for amf tower records
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(viridis)

PathToFluxData <- "./Data/FluxData/Regular"

site_csvs <- list.files(PathToFluxData, pattern = ".csv")
#paste flux data path in front of csv names instead of changing directory 
csv_dfs <- lapply(site_csvs, function(x) {
  df <- read.csv(file.path(PathToFluxData, x), na.strings = "-9999", skip = 2, sep = ",")
  
  #add site names
  siteID <- substr(x, 1, 10)
  df$siteID <- siteID
  return(df)
})

#select specific columns for each df (blah) because vars are uniquely named
#voi = site, time, air temp, soil temp, rel humidity, VPD, soil water, precip, and GPP
#2 = NA; timestamp stop
#5 = NA; wind direction
#3 = NA; CO2
#3 = NA; NETRAD
df_cols <- list(
  a = c(37, 1,3,4,18,19,30,17, 2),
  b = c(62, 1, 47, 53, 45, 15, 51, 2, 33),
  c = c(125, 1, 35, 40, 28, 2, 71, 56, 5),
  d = c(69, 1, 33, 54,56,21,42,17,45),
  e = c(104, 1, 6, 43, 42, 41, 28, 23, 39),
  f = c(39, 1, 24, 36, 23, 25, 2, 12, 3),
  g = c(105, 1, 25, 41, 26, 87, 103, 82, 2),
  h = c(92, 1, 30, 73, 39, 2, 68, 65, 85),
  i = c(61, 1, 4, 12, 14, 16, 51, 13, 2),
  j = c(48, 1, 17, 39, 18, 2, 31, 21, 3),
  k = c(106, 1, 46, 58, 75, 47, 48, 66, 71)
)

names(csv_dfs) <- names(df_cols)
voi_list <- lapply(names(df_cols), function(name) {
  selected_columns <- names(csv_dfs[[name]])[df_cols[[name]]]
  csv_dfs[[name]] %>%
    select(all_of(selected_columns))
})

#change filler columns to NA
test <- voi_list
voi_list_cln <- lapply(test, function(x) {
  x %>%
    mutate(across(any_of(c("TIMESTAMP_END", "CO2", "NETRAD", "WD")), ~ NA))
})

#make/apply common colnames
col_names <- c("Site", "Date", "TempAtmos", "TempSoil", "RelH", "VPD", "SoilWater", "Precip", "GPP")
voi_list_nmd <- lapply(voi_list_cln, function(x){
  setNames(x, col_names)
})

#combine sites to master df
all_sites_df <- do.call(rbind, voi_list_nmd)

#add columns for date components
all_sites_df$Date <- ymd_hm(all_sites_df$Date)
all_sites_df$Year <- year(all_sites_df$Date)

#calculate anomalies for all vars===============================================
voi = c("TempAtmos", "TempSoil", "RelH", "VPD", "SoilWater", "Precip", "GPP")
all_sites_df <- all_sites_df%>%
  group_by(Site)%>%
  mutate(across(all_of(voi), ~(mean(.x, na.rm = T)), .names = "avg_{.col}"))
z_scores <- all_sites_df %>%
  group_by(Site, Year) %>%
  summarize(across(all_of(voi), ~ (mean(.x, na.rm = TRUE) - get(paste0("avg_", cur_column()))) / sd(.x, na.rm = TRUE), 
                   .names = "z_{.col}"))%>%
  distinct()

#plotting=======================================================================
long_z_scores <- z_scores %>%
  pivot_longer(cols = starts_with("z_"),
               names_to = "Variable",
               values_to = "Anomaly")
#filter values- soil temp and water giving <-30 anomalies
long_z_filt <- long_z_scores %>%
  filter(Anomaly > -2,
         Year > 1999)

ggplot(long_z_filt, aes(x = Year, y = Anomaly, color = Variable)) +
  geom_point() +
  facet_wrap(~ Site) + 
  labs(title = "Ameriflux Site Climate Anomalies",
       x = "Year",
       y = "Z-score") +
  theme_minimal() +
  scale_color_manual(values = c("yellow", "pink", "orange", "blue", "green", "purple", "red"))
  

#filter values- soil temp and water giving <-30 anomalies

long_z_filt <- long_z_scores %>%
  filter(Anomaly > -8,
         Year > 1999,
         !Variable %in% c("z_TempAtmos", "z_Precip", "z_RelH", "z_GPP"))

ggplot(long_z_filt, aes(x = Year, y = Anomaly, color = Variable)) +
  geom_point() +
  facet_wrap(~ Site) + 
  labs(title = "Ameriflux Site Climate Anomalies",
       x = "Year",
       y = "Z-score") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "maroon", linetype = "dashed")+
  scale_color_manual(values = c("orange", "blue", "purple"))

#===============================================================================
#==========================SWC Anomalies: All Sensors===========================
#===============================================================================
#soil moisture with all data 
#select SWC and date columns for new list of SWC columns only
swc_list <- lapply(csv_dfs, function(x){
  x%>%
    select(siteID, TIMESTAMP_START, contains("SWC"))%>%
    rename(Date = TIMESTAMP_START)%>%
    mutate(Date = ymd_hm(Date))%>%
    mutate(Year = year(Date))%>%
    select(1:2, ncol(.), 3:(ncol(.) - 1))
})

#calc annual zscore for each sensor in the site profiles
#annual average calc: summarize across SWC columns, func = mean; rename to denote annual avg
swc_wAvg_list <- lapply(swc_list, function(x){
  x %>%
    group_by(Year) %>%
    summarize(siteID = first(siteID),
              Year = first(Year), 
              across(
                .cols = contains("SWC"),
                .fns = ~ mean(., na.rm = TRUE),
                .names = "avg_{col}"
              )) %>%
    ungroup()
})
#long term avg and sd
overall_stats <- lapply(swc_list, function(x){
  x %>%
    summarize(siteID = first(siteID),
              across(
                .cols = contains("SWC"),
                .fns = list(
                  longterm_mean = ~ mean(., na.rm = TRUE),
                  longterm_sd = ~ sd(., na.rm = TRUE)
                )
              )
    ) %>%
    distinct()
})
#calc zscore
swc_zs_list <- mapply(function(x, z){
  x %>%
    mutate(across(
      .cols = contains("avg_"),
      .fns = ~ {
        swc_col <- sub("avg_", "", cur_column())
        (.- z[[paste0(swc_col, "_longterm_mean")]]) /
          z[[paste0(swc_col, "_longterm_sd")]]
      },
      .names = "z_{col}"
    )) %>%
    distinct()
}, swc_wAvg_list, overall_stats, SIMPLIFY = FALSE)
print((swc_zs_list[[1]]))

#plotting swc_zs_list data=========================
#remove NGB site- no SWC data so is messing up the df pivotting for plotting
swc_zs_list <- swc_zs_list[-6]

#merge list and pivot for plotting; add sensor position by extracting second digit from column name
combined_long_data <- bind_rows(
  lapply(swc_zs_list, function(site_data) {
    site_data %>%
      pivot_longer(
        cols = starts_with("z_"),
        names_to = "SWC",
        values_to = "z_score",
        names_prefix = "z_"
      ) %>%
      filter(Year >1999)%>%
      mutate(
        siteID = site_data$siteID[1],  # Add siteID for identification
        sensor_position = ifelse(SWC %in% c("avg_SWC", "avg_SWC_PI_F_1", "avg_SWC_PI_1"), 1, 
                                 sub(".*?_(\\d+)_.*", "\\1", SWC))  # Set sensor_position to 1 for specific cases
      )
  })
)


#plot frame
ggplot(combined_long_data, aes(x = Year, y = z_score, color = sensor_position)) +
  geom_point(size = 1.2) +
  labs(title = "Annual Soil Moisture Anomalies",
       x = "Year",
       y = "Z-score",
       color = "Sensor Position") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ siteID)+
  geom_hline(yintercept = 0, color = "maroon", linetype = "dashed")


#=====================temp plot only===============================
long_z_scores <- z_scores %>%
  pivot_longer(cols = starts_with("z_"),
               names_to = "Variable",
               values_to = "Anomaly")
#filter values- soil temp and water giving <-30 anomalies
long_z_filt <- long_z_scores %>%
  filter(Anomaly > -2,
         Year > 1999,
         Variable == "z_TempAtmos")

ggplot(long_z_filt, aes(x = Year, y = Anomaly, color = Variable)) +
  geom_point() +
  facet_wrap(~ Site) + 
  labs(title = "Ameriflux Site Air Temp Anomalies",
       x = "Year",
       y = "Z-score") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "maroon", linetype = "dashed")+
  scale_color_manual(values = c("darkgreen"))

