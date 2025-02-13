library(tidyr)

BADM <- read.csv("./Data/AMF_AA-Net_BIF_CCBY4_20250201.csv")


test <- BADM%>%
  filter(VARIABLE == "SA")

AG_entries <- BADM %>%
  filter(grepl("AG_BIOMASS", VARIABLE) | grepl("SA", VARIABLE)) %>%
  filter(grepl("AG_BIOMASS", VARIABLE_GROUP) | VARIABLE_GROUP == "GRP_SA")

AG_reordered <- AG_entries %>%
  pivot_wider(names_from = VARIABLE,
              values_from = DATAVALUE)

age <- AG_reordered%>%
  select(SITE_ID, SA)%>%
  filter(!is.na(SA))%>%
  rename(SiteID = SITE_ID,
         StandAge = SA)

total_tree <- AG_reordered%>%
  filter(AG_BIOMASS_TREE_ORGAN == "Total")%>%
  select(SITE_ID, AG_BIOMASS_TREE)%>%
  rename(SiteID = SITE_ID)%>%
  mutate(AG_BIOMASS_TREE = as.numeric(AG_BIOMASS_TREE))

total_tree <- merge(age, total_tree)
DatwSiteBio <- merge(AllProdLongtermDat, total_tree)


ProdPlots <- inves %>%
  split(.$Product) %>%
  map(~ ggplot(.x, aes(x = BioLongtermAvg, y = AG_BIOMASS_TREE)) +
        geom_point() +
        labs(x = "Biomass Record Average", 
             y = "Site Tree Biomass (gC/m2)", 
             title = paste("Site Bio and Biomass Longterm Average:", unique(.x$Product))) +
        theme_minimal() +
        facet_wrap(~ IGBP_PI, scales = "free"))
#products are "xu", "esa_cci", "liu", "hfbs", "gfw", "gedi", "menlove", "icesat", "lt_gnn", "chopping", "nbcd"
#run code chunk above and then plots[[name]] to see in window
ProdPlots[["gedi"]]



inves <- DatwSiteBio%>%
  filter(IGBP_PI == "ENF")
