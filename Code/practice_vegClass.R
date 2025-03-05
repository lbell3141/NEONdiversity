woodherb_df <- IGBP_df %>%
  mutate(Group = case_when(
    IGBP %in% c("GRA") ~ "Herb Dominated",
    IGBP %in% c("SAV", "WSA", "OSH") ~ "Mixed",
    IGBP %in% c("DBF", "DNF", "EBF", "ENF", "MF") ~ "Wood Dominated",
    T ~ "Unclassified"))%>%
  mutate(Group = factor(Group, levels = c("Herb Dominated", "Mixed", "Wood Dominated", "Unclassified")))

vegclasslongterm <- merge(rec_avg, woodherb_df, by = "SiteID")%>%
  filter(Group != "Unclassified")



longtermGPPLAIigbp <- ggplot(vegclasslongterm, aes(LongtermLAI, LongtermGPP)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm", se = F, color = "maroon", alpha = 0.5)+
  stat_poly_eq(
    aes(label = paste(..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = T) +
  labs(x = "LAI Longterm Average", 
       y = "GPP Longterm Average") +
  theme_minimal() +
  facet_wrap(~ Group, scales = "free")
longtermGPPLAIigbp
