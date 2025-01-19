#relationship bt NDVI and SWC

#load libraries and dfs from flux_data.R and plot_MODIS.R
swc_df <- combined_long_data
ndvi_df <- zs_ndvi
temp_df <- z_scores

#sites of interest = KON, NR1, Prr, SRM
swc_df <- swc_df%>%
  filter(siteID %in% c("AMF_US-Kon", "AMF_US-NR1", "AMF_US-Prr", "AMF_US-SRM"))%>%
  select(siteID, Year, z_score, sensor_position)%>%
  filter(!is.nan(z_score))%>%
  mutate(siteID = ifelse(siteID == "AMF_US-Kon", "US.Kon", siteID),
         siteID = ifelse(siteID == "AMF_US-NR1", "US.NR1", siteID),
         siteID = ifelse(siteID == "AMF_US-Prr", "US.Prr", siteID),
         siteID = ifelse(siteID == "AMF_US-SRM", "US.SRM", siteID))%>%
  rename(site = siteID,
         year = Year,
         swc_zscore = z_score)

ndvi_df <- ndvi_df %>%
  filter(site %in% c("US.Kon", "US.NR1", "US.Prr", "US.SRM"))%>%
  select(site, year, zscore)%>%
  rename(ndvi_zscore = zscore)

temp_df <- temp_df%>%
  filter(Site %in% c("AMF_US-Kon", "AMF_US-NR1", "AMF_US-Prr", "AMF_US-SRM"))%>%
  mutate(Site = ifelse(Site == "AMF_US-Kon", "US.Kon", Site),
         Site = ifelse(Site == "AMF_US-NR1", "US.NR1", Site),
         Site = ifelse(Site == "AMF_US-Prr", "US.Prr", Site),
         Site = ifelse(Site == "AMF_US-SRM", "US.SRM", Site))%>%
  select(Site, Year, z_TempAtmos)%>%
  filter(!is.nan(z_TempAtmos),
         !is.na(z_TempAtmos))%>%
  rename(site = Site, 
         year = Year, 
         temp_zscore = z_TempAtmos)
  

plot_df <- merge(ndvi_df, swc_df)
plot_df <- merge(plot_df, temp_df)

#=========================plotting==============================================

ggplot(plot_df, aes(x = swc_zscore, y = ndvi_zscore, color = year)) +
  geom_point(size = 1.2) +
  labs(title = "",
       x = "Soil Moisture",
       y = "NDVI",
       color = "Year") +
  scale_color_viridis_c()+
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  facet_wrap(~ site)+
  geom_hline(yintercept = 0, color = "maroon", linetype = "dashed")+
  geom_vline(xintercept = 0, color = "maroon", linetype = "dashed")


#========================fit a linear model=====================================

lm_results <- plot_df %>%
  group_by(site) %>%
  do(model = lm(ndvi_zscore ~ swc_zscore, data = .)) %>%
  mutate(r_squared = summary(model)$r.squared)

r2_df <- lm_results %>%
  summarise(site, r_squared = round(r_squared, 2))
plot_df <- left_join(plot_df, r2_df, by = "site")

ggplot(plot_df, aes(x = swc_zscore, y = ndvi_zscore, color = sensor_position)) +
  geom_point(size = 1.2) +
  labs(title = "",
       x = "Soil Moisture (z-score)",
       y = "NDVI (z-score)",
       color = "Sensor Position") +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  facet_wrap(~ site) +
  geom_hline(yintercept = 0, color = "lightblue", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "lightblue", linetype = "dashed") +
  geom_smooth(method = "lm", aes(x = swc_zscore, y = ndvi_zscore), color = "maroon", size = 0.2, se = T, alpha = 0.2) +
  geom_text(data = r2_df, aes(x = Inf, y = Inf, label = paste("R² =", r_squared)),
            hjust = 1.1, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE)
