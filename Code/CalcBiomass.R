library(dplyr)
library(ggplot2)
library(purrr)
library(broom)

#MCD15A3H.061 MODIS Leaf Area Index/FPAR 4-Day Global 500m

PathtoTRYsum <- "./Data/TRY_PlantTraitDatabase/GrowthForm/GrowthForm.csv"
PathtoModisLAI <- "./Data/Annual_MODIS_LAI_Peak_Extraction.csv"
PathtoBiomassData <- "./Data/AnnualBiomassProducts.csv"
PathtoIGBP <- "./Data/SiteIGBPdomcov.csv"

#---Growth Form Summary Values--------------------------------------------------
TRY_dat <- read.csv(PathtoTRYsum)%>%
  rename(GrowthForm = OrigValueStr)
GF_summary <- TRY_dat%>%
  group_by(GrowthForm)%>%
  summarise(avgLCC = mean(avgCC, na.rm = T),
            seLCC = sd(avgCC, na.rm = TRUE) / sqrt(n()),
            maxLCC = max(avgCC, na.rm = T),
            minLCC = min(avgCC, na.rm = T))

#---Annual MODIS LAI by Site----------------------------------------------------
lai <- read.csv(PathtoModisLAI)%>%
  select(SITE_ID, year, max)%>%
  filter(!is.na(max))%>%
  rename(SiteID = SITE_ID,
         Year = year, 
         LAI = max)

#---Align LAI and Biomass Products----------------------------------------------
#need to convert to g/m2
biodat <- read.csv(PathtoBiomassData)%>%
  select(-ffp_radius) %>%
  filter(!is.na(agb_Mg_ha))%>%
  rename(Product = product,
         Year = year, 
         SiteID = SITE_ID,
         Biomass = agb_Mg_ha)

#pull products with estimates for date ranges instead of individual years of data
GediMenSub <- biodat%>%
  filter(Product %in% c("menlove", "gedi"))%>%
  select(-Year)
lairangeGedi <- lai%>%
  filter(Year %in% (2019:2023),
         LAI != 0)%>%
  group_by(SiteID)%>%
  mutate(LTlai = mean(LAI, na.rm = T),
         Product = "gedi")%>%
  select(SiteID, Product, LTlai)%>%
  distinct()
lairangeMenlove <- lai%>%
  filter(Year %in% (2009:2019),
         LAI != 0)%>%
  group_by(SiteID)%>%
  mutate(LTlai = mean(LAI, na.rm = T),
         Product = "menlove")%>%
  select(SiteID, Product, LTlai)%>%
  distinct()  
DateRangeLAI <- rbind(lairangeGedi,lairangeMenlove)  
GediMenBioLAI <- merge(DateRangeLAI, GediMenSub, by =c("SiteID", "Product"))%>%
  rename(avgProdBio = Biomass, 
         avgLAI = LTlai)%>%
  filter(avgProdBio != 0)

aligned_dat <- merge(lai, biodat, by = c("SiteID", "Year"))%>%
  filter(!Product %in% c("gedi", "menlove", "xu", "hfbs"))%>%
  group_by(Product, SiteID)%>%
  summarize(avgLAI = mean(LAI, na.rm = T), 
            avgProdBio = mean(Biomass, na.rm = T))%>%
  filter(avgProdBio != 0)

biolai_ff <- rbind(GediMenBioLAI, aligned_dat)%>%
  arrange(SiteID)

#---Add IGBP class and dominant cover type--------------------------------------
igbp <- read.csv(PathtoIGBP)%>%
  filter(IGBP != "CRO")
domcov_reclassed <- igbp %>%
  mutate(GrowthForm = case_when(
    Group == "Herb Dominated" ~ "grass",
    Group == "Mixed" ~ "shrub",
    Group == "Wood Dominated" ~ "tree",
    T ~ NA))%>%
  filter(!is.na(GrowthForm))%>%
  mutate(GrowthForm = factor(GrowthForm, levels = c("grass", "shrub", "tree")))
#connect cover class with LCC numbers from TRY database
site_LCC <- merge(domcov_reclassed, GF_summary, by = "GrowthForm")

#---Calculate Biomass (LAI*LCC)-------------------------------------------------
#combine LCC df with Biomass/LAI df 
calcframe <- merge(site_LCC, biolai_ff, by = "SiteID")%>%
  distinct()

calcBio <- calcframe%>%
  #convert from g/m2 to Mg/ha
  mutate(avgCalcBio = (avgLCC * avgLAI)/100,
         minCalcBio = (minLCC* avgLAI)/100,
         maxCalcBio = (maxLCC* avgLAI)/100)

#---Plot all data---------------------------------------------------------------  
CalcProdBio <- ggplot(calcBio, aes(x = avgProdBio, y = minCalcBio, color = Group)) +
  geom_point() +
  labs(x = "Above Ground Biomass Product (Mg/ha)", 
       y = "Calculated Leaf Biomass (Mg/ha)", 
       title = "",
       color = "Group") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
CalcProdBio

#---Plots by product with CI----------------------------------------------------
ProdPlots <- calcBio %>%
  split(.$Product) %>%
  map(~ {
    # Compute the linear model and confidence intervals for each group
    model_params <- .x %>%
      group_by(Group) %>%
      summarise(
        model = list(lm(avgCalcBio ~ avgProdBio, data = cur_data())),
        .groups = "drop"
      ) %>%
      mutate(
        slope = sapply(model, function(m) coef(m)[2]),
        se_slope = sapply(model, function(m) summary(m)$coefficients[2, 2]),
        ci_lower = slope - 1.96 * se_slope,
        ci_upper = slope + 1.96 * se_slope
      )
    
    # Create a dataset for the confidence intervals and predictions
    plot_data <- .x %>%
      group_by(Group) %>%
      do({
        model <- lm(avgCalcBio ~ avgProdBio, data = .)
        data.frame(
          avgProdBio = seq(min(.$avgProdBio), max(.$avgProdBio), length.out = 100),
          avgCalcBio = predict(model, newdata = data.frame(avgProdBio = seq(min(.$avgProdBio), max(.$avgProdBio), length.out = 100))),
          ci_lower = predict(model, newdata = data.frame(avgProdBio = seq(min(.$avgProdBio), max(.$avgProdBio), length.out = 100)), se.fit = TRUE)$fit - 1.96 * predict(model, newdata = data.frame(avgProdBio = seq(min(.$avgProdBio), max(.$avgProdBio), length.out = 100)), se.fit = TRUE)$se.fit,
          ci_upper = predict(model, newdata = data.frame(avgProdBio = seq(min(.$avgProdBio), max(.$avgProdBio), length.out = 100)), se.fit = TRUE)$fit + 1.96 * predict(model, newdata = data.frame(avgProdBio = seq(min(.$avgProdBio), max(.$avgProdBio), length.out = 100)), se.fit = TRUE)$se.fit
        )
      }) %>%
      ungroup()
    
    # Create the plot with confidence intervals displayed as ribbons
    ggplot(.x, aes(x = avgProdBio, y = avgCalcBio)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "maroon") +
      # Add the shaded confidence interval ribbon
      geom_ribbon(data = plot_data, aes(x = avgProdBio, ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
      labs(
        x = "Above Ground Biomass Product (Mg/ha)", 
        y = "Calculated Leaf Biomass (Mg/ha)", 
        title = paste("Leaf Biomass Allocation:", unique(.x$Product))
      ) +
      theme_minimal() +
      facet_wrap(~ Group, scales = "free")
  })
ProdPlots[["liu"]]

#---Plot: compare slopes--------------------------------------------------------
slopes_df <- calcBio %>%
  group_by(Product, Group) %>%
  summarise(
    avg_slope = coef(lm(avgCalcBio ~ avgProdBio))[2],
    min_slope = coef(lm(minCalcBio ~ avgProdBio))[2],
    max_slope = coef(lm(maxCalcBio ~ avgProdBio))[2],
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(avg_slope, min_slope, max_slope), 
               names_to = "Slope_Type", 
               values_to = "Slope_Value")

ggplot(slopes_df
       %>%filter(Slope_Type == "avg_slope")
       , aes(x = Product, y = Slope_Value, color = Group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_line(aes(group = interaction(Product, Group)), position = position_dodge(width = 0.5)) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "",
       x = "Product",
       y = "Slope",
       color = "Group")

slope_table <- slopes_df %>%
  arrange(Product, Group, Slope_Type)
#write.csv(slope_table, "slope_values.csv", row.names = FALSE)



