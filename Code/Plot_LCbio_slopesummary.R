library(gridExtra)
library(kableExtra)
group_slope_summary <- calcBio %>%
  split(.$Product) %>%
  map_df(~ {
    .x %>%
      nest_by(Group) %>%
      mutate(
        model = list(lm(avgCalcBio ~ avgProdBio, data = data)),
        slope = coef(model)[2],
        se_slope = summary(model)$coefficients[2, 2],
        ci_lower = slope - 1.96 * se_slope,
        ci_upper = slope + 1.96 * se_slope
      ) %>%
      ungroup() %>%
      select(Group, slope, se_slope, ci_lower, ci_upper) %>%
      mutate(Product = unique(.x$Product))
  })
group_slope_summary %>%
  kable() %>%
  kable_styling() %>%
  column_spec(1, bold = TRUE)


library(ggplot2)

# Plot the summary values
ggplot(group_slope_summary %>% filter(Group %in% c("Herb Dominated", "Wood Dominated")),
       aes(x = Product, y = slope, color = Group)) +
  geom_point(size = 4) +  # Points for slope
  geom_errorbar(aes(ymin = slope - se_slope, ymax = slope + se_slope), width = 0.2) +
  scale_color_manual(values = c("Herb Dominated" = "forestgreen", "Wood Dominated" = "chocolate4")) +
  labs(x = "Product", y = "Slope (LC/AGB)", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#add back mixed systems
ggplot(group_slope_summary, aes(x = Product, y = slope, color = Group)) +
  geom_point(size = 4) +  # Points for slope
  geom_errorbar(aes(ymin = slope - se_slope, ymax = slope + se_slope), width = 0.2) +
  scale_color_manual(values = c("Herb Dominated" = "forestgreen", "Wood Dominated" = "chocolate4", "Mixed" = "cyan3")) +
  labs(x = "Product", y = "Slope (LC/AGB)", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

