library(terra)
library(dplyr)
library(ggplot2)

PathtoGlobalIndex <- "./Data/3506_70_Dataset/w3_tile_joint_sr1000.tif"
PathtoColocTowers <- "./Data/colocated_ameriflux_neon_towers.csv"

sab_index <- rast(PathtoGlobalIndex)
coloctowers <- read.csv(PathtoColocTowers)

#extract pixel values at tower lat/lon positions (12 US towers)
coord_df <- coloctowers[,c(6,7)]
coords <- vect(coord_df, geom = c("Longitude","Latitude"), crs = "EPSG:4326")
coords <- project(coords, crs(sab_index))
div_vals <- extract(sab_index, coords)
coloctowers <- cbind(coloctowers, div_vals)
coloctowers <- coloctowers%>%
  mutate(ID = NULL)%>%
  rename(Diversity = w3_tile_joint_sr1000)%>%
  arrange(Diversity)

coloctowers <- coloctowers[-9,]
coloctowers <- coloctowers %>%
  mutate(Ameriflux_Site_ID = factor(Ameriflux_Site_ID, levels = Ameriflux_Site_ID[order(Diversity)]))


sabdiv_plot <- ggplot(data = coloctowers, mapping = aes(x = Ameriflux_Site_ID, y = Diversity))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  labs(x = "Ameriflux Site ID", y = "Diversity Index Value", title = "Sabatini Diversity Value for Ameriflux Sites")+
  theme_minimal()
