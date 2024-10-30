#temp/NDVI regression
#all_sites_df from flux_data.R

base_data <- all_sites_df%>%
  select(Site, Year, TempAtmos)%>%
  group_by(Site, Year)%>%
  mutate(TempAvg = mean(TempAtmos, na.rm = T))%>%
  ungroup()%>%
  group_by(Site)%>%
  mutate(LTA = mean(TempAtmos, na.rm = T),
         LTSD = sd(TempAtmos, na.rm = T))%>%
  ungroup()
tempZS_dat <- base_data%>%
  group_by(Site, Year)%>%
  summarize(zscore_temp = (TempAvg - LTA)/LTSD)%>%
  distinct()%>%
  filter(Site %in% c("AMF_US-Kon", "AMF_US-NR1", "AMF_US-Prr", "AMF_US-SRM"))


tempZS_dat <- tempZS_dat%>%
  mutate(Site = ifelse(Site == "AMF_US-Kon", "US.Kon", Site),
         Site = ifelse(Site == "AMF_US-NR1", "US.NR1", Site),
         Site = ifelse(Site == "AMF_US-Prr", "US.Prr", Site),
         Site = ifelse(Site == "AMF_US-SRM", "US.SRM", Site))%>%
  rename(site = Site,
         year = Year,
         temp_zscore = zscore_temp)

plot_df <- merge(ndvi_df, tempZS_dat)



lm_results <- plot_df %>%
  group_by(site) %>%
  do(model = lm(ndvi_zscore ~ temp_zscore, data = .)) %>%
  mutate(r_squared = summary(model)$r.squared)

r2_df <- lm_results %>%
  summarise(site, r_squared = round(r_squared, 2))
plot_df <- left_join(plot_df, r2_df, by = "site")

ggplot(plot_df, aes(x = temp_zscore, y = ndvi_zscore, color = year)) +
  geom_point(size = 1.2) +
  labs(title = "",
       x = "Air Temp (z-score)",
       y = "NDVI (z-score)",
       color = "Year") +
  scale_color_viridis_c() +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  facet_wrap(~ site, ncol = 2) +
  geom_hline(yintercept = 0, color = "lightblue", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "lightblue", linetype = "dashed") +
  geom_smooth(method = "lm", aes(x = temp_zscore, y = ndvi_zscore), color = "maroon", size = 0.2, se = T, alpha = 0.2) +
  geom_text(data = r2_df, aes(x = Inf, y = Inf, label = paste("R² =", r_squared)),
            hjust = 1.1, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE)
