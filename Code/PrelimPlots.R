ggplot(AccRainDF, aes(x = RecordNum, y = AccRain)) +
  geom_point(color = "black") +
  labs(x = "Record Number (Oct-Sep)", y = "Accumulated Rainfall", title = "Accumulated Rainfall with Breakpoints") +
  geom_vline(xintercept = bps_position[1], color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = bps_position[2], color = "red", linetype = "dashed", size = 1) +
  #geom_vline(xintercept = bps_position[3], color = "red", linetype = "dashed", size = 1) +
  theme_minimal()
testdat 
library(ggplot2)

ggplot(final_df, aes(x = WinterPrecipZScore, y = GPPZScore, color = PrecipCondition)) +
  geom_point(size = 10) +
  theme_minimal() +
  labs(
    x = "Summer Precipitation Z-Score",
    y = "GPP Z-Score",
    color = "Precipitation Condition"
  )

test <- dfs_rainy_season[[3]]
test <- test%>%
  filter(WaterYr==2008)
ggplot(test, aes(x = WYrecord, y = P_F, color = Season)) +
  geom_point() +
  labs(x = "Record Number (Oct-Sep)", 
       y = "Accumulated Rainfall", 
       title = "Accumulated Rainfall with Precip Breakpoints",
       color = "Season") +
  theme_minimal()
