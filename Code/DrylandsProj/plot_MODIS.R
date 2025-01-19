#plotting MODIS pull

library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

pathtoMODIS <- "./Data/MODIS_NDVI_extraction.csv"
modis <- read.csv(pathtoMODIS)

modis$date <- as.Date(modis$date)

#make dates columns and sites rows
mod_mod <- modis%>%
  pivot_longer(cols = -date, names_to = "site", values_to = "NDVI")

ggplot(data = mod_mod, aes(x = date, y = NDVI, color = site, group = site)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "NDVI", color = "Ameriflux Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")
  
single_site <- mod_mod%>%
  filter(site == "US.Ha1")
ggplot(data = single_site, aes(x = date, y = NDVI))+
  geom_point()+
  geom_line()+
  theme_minimal()

#annual anomalies 
#create year col from date 
mod_mod$date = ymd(mod_mod$date)
mod_mod$year <-year(mod_mod$date)
  
an_avg <- mod_mod%>%
  group_by(site, year)%>%
  summarize(avg_ndvi = mean(NDVI, na.rm = T))
ggplot(data = an_avg, aes(x = year, y = avg_ndvi, color = site, group = site)) +
  geom_point()+
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "NDVI", color = "Ameriflux Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#z scores (x-m/sd)
zs_ndvi <- an_avg%>%
  group_by(site)%>%
  mutate(full_mean = mean(avg_ndvi))%>%
  mutate(full_sd = sd(avg_ndvi))
zs_ndvi <- zs_ndvi %>%
  group_by(site, year)%>%
  mutate(zscore = ((avg_ndvi - full_mean) /full_sd))
ggplot(data = zs_ndvi, aes(x = year, y = zscore, color = site, group = site)) +
  geom_point() +
  geom_line()+
  theme_minimal() +
  labs(x = "Date", y = "NDVI Anomalies", color = "Ameriflux Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ site)  

#plot temp, swc, and VPD anomalies