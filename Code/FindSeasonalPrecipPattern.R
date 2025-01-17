#currently static dates for seasons, but will implement break point analysis on rainfall accumulation curve

#Script to build a data frame of seasonal precip anomalies for a flux site
  # 1. identify date and precip column
  # 2. bin precip by winter and summer seasons 
  # 3. calculate summer and winter precip zscores for each year
  # 4. denote positive values as wet and negative values as dry
  # 5. clean final df

#===============================================================================
#Load Necessary Packages 
#===============================================================================
library(lubridate)
library(dplyr)
library(tidyverse) 
#===============================================================================
#Identify Date and Precip Columns in Flux Data
#===============================================================================
flux_data <- read.csv("./Data/FluxData/Regular/AMF_US-SRM_BASE_HH_28-5.csv", na.strings = -9999, sep = ",", skip = 2)

flux_data$TIMESTAMP_START <- ymd_hm(flux_data$TIMESTAMP_START)

columns_of_interest <- flux_data %>% 
  mutate(Year = year(TIMESTAMP_START),
            Month = month(TIMESTAMP_START),
            Day = day(TIMESTAMP_START),
            Day_of_Year = yday(TIMESTAMP_START))%>%
  group_by(Year, Month, Day)%>%
  mutate(Precip =sum(P, na.rm = T))%>% #may need to change to P_PI
  select(Year, Month, Day, Day_of_Year, Precip) #note precip is in mm
columns_of_interest <- distinct(columns_of_interest)
#plot(columns_of_interest$Day_of_Year, columns_of_interest$Precip)

#===============================================================================
#Label Water Years and Define Winter/Summer Seasons (Static Approach)
#===============================================================================
#DayOfYearPrecip <- columns_of_interest%>%
 # group_by(Day_of_Year)%>%
  #summarize(AveragePrecip = mean(Precip, na.rm = T))
#DayOfYearPrecip <- DayOfYearPrecip%>%
 # mutate(sinDOY = sin(2*pi*Day_of_Year/366),
        # cosDOY = cos(2*pi*Day_of_Year/366)) #transform to sin/cos so dates are continuous (ie don't start/stop at 1/366)
water_year <- columns_of_interest%>%
  mutate(water_year = ifelse(Month > 9, Year + 1, Year))

SeasonsDOY <- water_year%>%
  mutate(Season = case_when(Day_of_Year >=166 & Day_of_Year <= 288 ~ "Summer", Day_of_Year >=305 | Day_of_Year <= 59 ~ "Winter", T ~ "DrySeason"))%>%
  mutate(sinDOY = sin(2*pi*Day_of_Year/366),
         cosDOY = cos(2*pi*Day_of_Year/366))

WinSumPpt <- SeasonsDOY%>%
  filter(Season != "DrySeason")
#===============================================================================
#Calculate Seasonal Z-Scores
#===============================================================================
AvgPrecip <- WinSumPpt%>%
  group_by(water_year, Season)%>%
  summarize(TotalAnnualPrecip = sum(Precip, na.rm = T))

longterm_values <- AvgPrecip%>%
  group_by(Season)%>%
  summarize(longterm_avg = mean(TotalAnnualPrecip, na.rm = T),
         longterm_sd = sd(TotalAnnualPrecip, na.rm = T))
calc_df <- merge(AvgPrecip, longterm_values, by = "Season")
zscores <- calc_df%>%
  group_by(water_year, Season)%>%
  summarize(zscore = (TotalAnnualPrecip - longterm_avg)/longterm_sd)

#===============================================================================
#Clean Up Dataframe
#===============================================================================
result_df <- zscores%>%
  pivot_wider(names_from = Season, 
              values_from = zscore)%>%
  select(water_year, Winter, Summer)

result_df <- result_df%>%
  rename(SummerPrecipZscore = Summer, 
         WinterPrecipZscore = Winter,
         WaterYR = water_year)%>%
  mutate(PrecipCondition = case_when(
  WinterPrecipZscore < 0 & SummerPrecipZscore < 0 ~ "DD",
  WinterPrecipZscore < 0 & SummerPrecipZscore > 0 ~ "DW",
  WinterPrecipZscore > 0 & SummerPrecipZscore < 0 ~ "WD",
  WinterPrecipZscore > 0 & SummerPrecipZscore > 0 ~ "WW"))
