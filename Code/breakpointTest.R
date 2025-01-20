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
#===============================================================================
#===============================================================================
#===============================================================================
test_list <- list(
  "X" = data.frame(
    WaterYr = rep(2003:2005, each = 50),
    P_F = runif(150),
    C = runif(150)
  ),
  "Y" = data.frame(
    WaterYr = rep(2003:2005, each = 50),
    P_F = runif(150),
    C = runif(150)
  )
)

NestedBrkPts_dfs <- lapply(test_list, function(i) {
  i %>%
    group_by(WaterYr) %>%
    summarize(
      BreakPoints = N_break_point(P_F, n_max = 2, n_period = 15, seed_set = 4557)
    )})

ExtractedBrkPts_dfs <- lapply(NestedBrkPts_dfs, function(i) {
  lapply(seq_along(i$BreakPoints), function(index) {
    j <- i$BreakPoints[[index]]
    
    # Extract the breaks and assign BP1 and BP2
    BP1 <- j$breaks[[2]][1]
    BP2 <- j$breaks[[2]][2]
    
    # Extract WaterYr (since each element in i$BreakPoints corresponds to a WaterYr)
    WaterYr <- i$WaterYr[index]
    
    # Return a data frame with BP1, BP2, and WaterYr for each nested BreakPoint
    data.frame(WaterYr = WaterYr, BP1 = BP1, BP2 = BP2)
  })
})
BrkPts_bind <- lapply(ExtractedBrkPts_dfs, function(i){
  i%>%
    bind_rows()
})
