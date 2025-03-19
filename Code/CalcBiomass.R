library(dplyr)
library(ggplot2)
library(purrr)

#MCD15A3H.061 MODIS Leaf Area Index/FPAR 4-Day Global 500m

PathtoTRYsum <- "./Data/TRY_PlantTraitDatabase/GrowthForm/GrowthForm.csv"
PathtoModisLAI <- "./Data/Annual_MODIS_LAI_Extraction.csv"
PathtoBiomassData <- "./Data/AnnualBiomassProducts.csv"
PathtoIGBP <- "./Data/SiteIGBPdomcov.csv"

#---Growth Form Summary Values--------------------------------------------------
TRY_dat <- read.csv(PathtoTRYsum)%>%
  rename(GrowthForm = OrigValueStr)
GF_summary <- TRY_dat%>%
  group_by(GrowthForm)%>%
  summarise(avgLCC_GF = mean(avgCC, na.rm = T),
            seLCC_GF = sd(avgCC, na.rm = TRUE) / sqrt(n()))

#---Annual MODIS LAI by Site----------------------------------------------------
lai <- read.csv(PathtoModisLAI)%>%
  select(SITE_ID, year,mean)%>%
  rename(SiteID = SITE_ID,
         Year = year, 
         avgLAI = mean)

#---Biomass Products------------------------------------------------------------
#need to convert to g/m2
biodat <- read.csv(PathtoBiomassData)%>%
  select(-ffp_radius) %>%
  filter(!is.na(agb_Mg_ha))%>%
  rename(Product = product,
         Year = year, 
         SiteID = SITE_ID,
         Biomass = agb_Mg_ha)
aligned_dat <- merge(lai, biodat, by = c("SiteID", "Year"))%>%
  filter(Product %in% c("esa_cci", "liu"))

#---Calculate Biomass (LAI*LCC)-------------------------------------------------
igbp <- read.csv(PathtoIGBP)
domcov_reclassed <- igbp %>%
  mutate(GrowthForm = case_when(
    Group == "Herb Dominated" ~ "grass",
    Group == "Mixed" ~ "shrub",
    Group == "Wood Dominated" ~ "tree",
    T ~ NA))%>%
  filter(!is.na(GrowthForm))%>%
  mutate(GrowthForm = factor(GrowthForm, levels = c("grass", "shrub", "tree")))

site_LCC <- merge(domcov_reclassed, GF_summary, by = "GrowthForm")
calcframe <- merge(site_LCC, aligned_dat, by = "SiteID")%>%
  group_by(SiteID, Product)%>%
  summarize(Group = Group,
            avgLCC = mean(avgLCC_GF, na.rm = T),
            seLCC = sd(avgLCC_GF, na.rm = T) / sqrt(n()),
            avgLAI = mean(avgLAI, na.rm = T),
            avgProdBio = mean(Biomass, na.rm = T))%>%
  distinct()
calcBio <- calcframe%>%
  mutate(avgCalcBio = (avgLCC * avgLAI)/100) #convert from g/m2 ot Mg/ha

CalcProdBio <- ggplot(calcBio, aes(x = avgProdBio, y = avgCalcBio, color = Group)) +
  geom_point() +
  labs(x = "Above Ground Biomass Product (Mg/ha)", 
       y = "Calculated Leaf Biomass (Mg/ha)", 
       title = "",
       color = "Group") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
CalcProdBio


ProdPlots <- calcBio %>%
  split(.$Product) %>%
  map(~ {
    #slope and r2 for each group in each product
    model_params <- .x %>%
      group_by(Group) %>%
      summarise(
        model = list(lm(avgCalcBio ~ avgProdBio, data = cur_data())),
        .groups = "drop"
      ) %>%
      mutate(
        r2 = sapply(model, function(m) summary(m)$r.squared),
        slope = sapply(model, function(m) coef(m)[2])
      )
    
    ggplot(.x, aes(x = avgProdBio, y = avgCalcBio)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "maroon") +
      labs(x = "Above Ground Biomass Product (Mg/ha)", 
           y = "Calculated Leaf Biomass (Mg/ha)", 
           title = paste("Leaf Biomass Allocation:", unique(.x$Product))) +
      theme_minimal() +
      facet_wrap(~ Group, scales = "fixed") +
      geom_text(data = model_params, aes(
        x = min(.x$avgProdBio, na.rm = TRUE), 
        y = max(.x$avgCalcBio, na.rm = TRUE),
        label = paste0("R² = ", round(r2, 3), "\nSlope = ", round(slope, 3))
      ), hjust = 0, vjust = 1, size = 4, color = "black")
  })
ProdPlots[["esa_cci"]]


