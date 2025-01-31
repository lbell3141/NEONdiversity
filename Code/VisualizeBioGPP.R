#This script visualizes the data frames from BiomassGPPAnnualData.R

#===============================================================================
#Load Necessary Packages
#===============================================================================
library(ggplot2)
library(dplyr)
library(purrr)
library(viridis)
#===============================================================================
#Visualize Longterm Flux/Biomass Averages by Product
#===============================================================================
Longterm_plot <- ggplot(AllProdLongtermDat, aes(x = BioLongtermAvg, y = GPPLongtermAvg, color = IGBP_name)) +
  geom_point() +
  labs(x = "Biomass Record Average", 
       y = "GPP Record Average", 
       title = "Biomass-GPP Longterm Average Comparison",
       color = "IGBP_name") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot
#===============================================================================
#Visualize Longterm Flux/Biomass Averages by IGBP
#===============================================================================
Longterm_plot <- ggplot(AllProdLongtermDat, aes(x = BioLongtermAvg, y = GPPLongtermAvg, color = Product)) +
  geom_point() +
  labs(x = "Biomass Record Average", 
       y = "GPP Record Average", 
       title = "Biomass-GPP Longterm Average Comparison",
       color = "Product") +
  theme_minimal() +
  facet_wrap(~ IGBP_name, scales = "free")
Longterm_plot

#===============================================================================
#Visualize Longterm Flux/Biomass Averages by IGBP for Each Product
#===============================================================================
ProdPlots <- AllProdLongtermDat %>%
  split(.$Product) %>%
  map(~ ggplot(.x, aes(x = BioLongtermAvg, y = GPPLongtermAvg)) +
        geom_point() +
        labs(x = "Biomass Record Average", 
             y = "GPP Record Average", 
             title = paste("Biomass-GPP Longterm Average:", unique(.x$Product))) +
        theme_minimal() +
        facet_wrap(~ IGBP_name, scales = "free"))
#products are "xu", "esa_cci", "liu", "hfbs", "gfw", "gedi", "menlove", "icesat", "lt_gnn", "chopping", "nbcd"
#run code chunk above and then plots[[name]] to see in window
ProdPlots[["xu"]]
#===============================================================================
#Visualize Longterm Flux/Biomass Averages by Aridity
#===============================================================================
Longterm_plot <- ggplot(LongDatwAI, aes(x = BioLongtermAvg, y = GPPLongtermAvg, color = AI)) +
  geom_point() +
  scale_color_gradientn(
    colors = c("red", "yellow", "blue"),  
    values = c(0, 0.2, 1),
    limits = c(0, 10))+
  labs(x = "Biomass Record Average", 
       y = "GPP Record Average", 
       title = "Biomass-GPP Longterm Average Comparison",
       color = "AI") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot

ProdPlots <- LongDatwAI %>%
  split(.$Product) %>%
  map(~ ggplot(.x, aes(x = BioLongtermAvg, y = GPPLongtermAvg, color = AI)) +
        geom_point() +
        scale_color_gradientn(
          colors = c("red", "yellow", "blue"),  
          values = c(0, 0.2, 1),
          limits = c(0, 10))+
        labs(x = "Biomass Record Average", 
             y = "GPP Record Average", 
             title = paste("Biomass-GPP Longterm Average:", unique(.x$Product))) +
        theme_minimal() +
        facet_wrap(~ IGBP_name, scales = "free"))
#products are "xu", "esa_cci", "liu", "hfbs", "gfw", "gedi", "menlove", "icesat", "lt_gnn", "chopping", "nbcd"
#run code chunk above and then plots[[name]] to see in window
ProdPlots[["nbcd"]]
#===============================================================================
#Visualize GPP/Bio by Aridity
#===============================================================================
Longterm_plot <- ggplot(LongDatwAI, aes(x = AI, y = BioLongtermAvg)) +
  geom_point() +
  labs(x = "Aridity", 
       y = "Biomass Record Average", 
       title = "Biomass-Aridity Longterm Average Comparison") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot

Longterm_plot <- ggplot(LongDatwAI, aes(x = AI, y = GPPLongtermAvg)) +
  geom_point() +
  labs(x = "Aridity", 
       y = "GPP Record Average", 
       title = "GPP-Aridity Longterm Average Comparison") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
Longterm_plot
#===============================================================================
#Visualize Annual Zscore Values for Multi-Year Products
#===============================================================================
AllProdAllSites <- ggplot(gppbio, aes(x = BiomassZscore, y = GPPZScore, color = Product)) +
  geom_point() +
  labs(x = "Biomass Z-Score", 
       y = "GPP Z-Score", 
       title = "Biomass-GPP Comparison",
       color = "Product") +
  theme_minimal()
AllProdAllSites

AllSitesByProd <- ggplot(gppbio, aes(x = BiomassZscore, y = GPPZScore, color = IGBP_name)) +
  geom_point() +
  labs(x = "Biomass Z-Score", 
       y = "GPP Z-Score", 
       title = "Biomass-GPP Comparison",
       color = "IGBP_name") +
  theme_minimal() +
  facet_wrap(~ Product, scales = "free")
AllSitesByProd

ProdPlotsAnnual <- gppbio %>%
  split(.$Product) %>%
  map(~ ggplot(.x, aes(x = BiomassZscore, y = GPPZScore)) +
        geom_point() +
        labs(x = "Biomass Annual Z Score", 
             y = "GPP Annual Z Score", 
             title = paste("Biomass-GPP Annual Z Score Comparison:", unique(.x$Product))) +
        theme_minimal() +
        facet_wrap(~ IGBP_name, scales = "free"))
#products are "xu", "esa_cci", "liu, "lt_gnn", "chopping"
#run code chunk above and then plots[[name]] to see in window
ProdPlotsAnnual[["chopping"]]


