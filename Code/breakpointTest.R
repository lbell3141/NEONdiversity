#testing multiple breakpoints in precip data

#uses package BreakPoints
#columns_of_interest from FindSeasonalPrecipPattern

library(BreakPoints)

precip <- water_year
precip_full <- precip%>%
  ungroup()%>%
  mutate(RecordNum = seq(1, nrow(precip), 1))
#plot(precip_full$RecordNum, precip_full$Precip)

bptest <- precip_full%>%
  filter(water_year == 2007)
plot(bptest$RecordNum, bptest$Precip)
#N_b = fix number of breakpoints (N_b = 4?)
#V = precip timeseries
#V has length N; N = 17520

#Using the selected breakpoint test try to detect a breakpoint in V 
#then associate the p-value to Nb = 1.
AccRainDF <- bptest%>%
  mutate(AccRain = cumsum(Precip))%>%
  mutate(RecordNum = seq(1, nrow(AccRainDF), 1))

bps<- N_break_point(bptest$Precip, n_max = 2, n_period = 15, seed_set = 4557)
bps_position <- bps[[1]]$breaks[[2]]

ggplot(AccRainDF, aes(x = RecordNum, y = AccRain)) +
  geom_point(color = "black") +
  labs(x = "Record Number (Oct-Sep)", y = "Accumulated Rainfall", title = "Accumulated Rainfall with Breakpoints") +
  geom_vline(xintercept = bps_position[1], color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = bps_position[2], color = "red", linetype = "dashed", size = 1) +
  #geom_vline(xintercept = bps_position[3], color = "red", linetype = "dashed", size = 1) +
  theme_minimal()
