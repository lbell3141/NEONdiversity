#Add Biomass
data <- read.csv("./Data/PrecipFlux.csv")
library(ggplot2)
data <- data%>%
  filter(WaterYR != 2022)
ggplot(data = data, aes(x = SummerPrecipZscore, y = SummerCarbonZscore))+
  geom_point(aes(color = PrecipCondition))+
  theme_minimal()
