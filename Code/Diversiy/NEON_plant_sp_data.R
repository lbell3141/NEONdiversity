#NEON richness quant
library(ggplot2)
library(dplyr)

PathtoPlantFolder <- "./Data/NEON_presence-cover-plant/plant_survey_csvs"

#list subfolder
site_csvs <- list.files(PathtoPlantFolder, recursive = TRUE)

#read csvs and merge into one
setwd(PathtoPlantFolder)

csv_dfs <- lapply(site_csvs, read.csv)

all_sites <- do.call(rbind, csv_dfs)

#site richness
site_coi <- all_sites%>%
  select(siteID, 
         decimalLatitude,
         decimalLongitude,
         plotType,
         nlcdClass,
         endDate,
         scientificName)%>%
  arrange(siteID)

site_rich <- site_coi%>%
  group_by(siteID, endDate)%>%
  summarize(richness = length(unique(scientificName)), .groups = "drop")
site_avgrich<- site_rich%>%
    group_by(siteID)%>%
  summarize(avgRich = mean(richness))%>%
  arrange(avgRich)%>%
  mutate(siteID = factor(siteID, levels = siteID))


ggplot(site_avgrich, aes(x = siteID, y = avgRich)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "NEON Site ID", y = "Average Richness") +
  theme_minimal()

site_avgrich <- site_avgrich%>%
  mutate(amfID = c("US-Kon2", "US-Prr", "US-Bar", "US-EML", "US-NGB", "US-NR1", "US-SP1", "US-SRM", "US-Ha1", "US-KFS", "US-Kon1", "US-Syv"))

ggplot(site_avgrich, aes(x = amfID, y = avgRich)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Ameriflux Site ID", y = "Average Richness") +
  theme_minimal()

