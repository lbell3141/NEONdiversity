dat <- read.csv("C:/Users/lindseybell/Downloads/AMF_US-SRM_FLUXNET_SUBSET_2004-2023_3-6/AMF_US-SRM_FLUXNET_SUBSET_HH_2004-2023_3-6.csv", na.strings = "-9999")
library(dplyr)
library(ggplot2)
library(lubridate)
dat_time <- dat%>%
  mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START),
         year = year(TIMESTAMP_START),
         month = month(TIMESTAMP_START),
         doy = yday(TIMESTAMP_START))
avg_ppt <- dat_time%>%
  group_by(month)%>%
  summarize(ppt = mean(P_F, na.rm = T))
ggplot(avg_ppt, aes(x = month, y = ppt))+
  geom_point()+
  scale_color_viridis_d()+
  labs(x = "day of year",
       y = "rainfall")+
  theme_minimal()