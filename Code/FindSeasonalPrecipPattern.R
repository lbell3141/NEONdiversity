#basic stats to cluster DOY rainfall for sum and win seasons
#working w dummy numbers rn ^
#would be better to sum rainfall through DOY for each year and each season rather than average it

#Script to build a dataframe of seasonal precip anomalies for a flux site
  # 1. identify date and precip column
  # 2. bin precip by winter and summer seasons using some sort of natural breaks stat
  # 3. calculate summer and winter precip zscores for each year
  # 4. denote positive values as wet and negaive values as dry
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
  mutate(Precip = mean(P, na.rm = T))%>% #may need to change to P_PI
  select(Year, Month, Day, Day_of_Year, Precip)

#plot(columns_of_interest$Day_of_Year, columns_of_interest$Precip)

#===============================================================================
#Identify Summer Rainy Season Dates and Winter Rainy Season Dates
#===============================================================================
DayOfYearPrecip <- columns_of_interest%>%
  group_by(Day_of_Year)%>%
  summarize(AveragePrecip = mean(Precip, na.rm = T))
DayOfYearPrecip <- DayOfYearPrecip%>%
  mutate(sinDOY = sin(2*pi*Day_of_Year/366),
         cosDOY = cos(2*pi*Day_of_Year/366)) #transform to sin/cos so dates are continuous (ie don't start/stop at 1/366)

#===============================================================================
#Calculate Seasonal Z-Scores
#===============================================================================
#dummy values: summer from DOY 165-250, winter from 340-90   
SeasonsDOY <- columns_of_interest%>%
  mutate(Season = case_when(Day_of_Year >=165 & Day_of_Year <= 250 ~ "Summer", T ~ "Winter"))%>%
  mutate(sinDOY = sin(2*pi*Day_of_Year/366),
         cosDOY = cos(2*pi*Day_of_Year/366))
AvgPrecip <- SeasonsDOY%>%
  group_by(Year, Season)%>%
  summarize(AnnualPrecip = mean(Precip, na.rm = T))
longterm_values <- SeasonsDOY%>%
  group_by(Season)%>%
  summarize(longterm_avg = mean(Precip, na.rm = T),
         longterm_sd = sd(Precip, na.rm = T))
calc_df <- merge(AvgPrecip, longterm_values, by = "Season")
zscores <- calc_df%>%
  group_by(Year, Season)%>%
  summarize(zscore = (AnnualPrecip - longterm_avg)/longterm_sd)

#===============================================================================
#Clean Up Dataframe
#===============================================================================
result_df <- zscores%>%
  pivot_wider(names_from = Season, 
              values_from = zscore)%>%
  select(Year, Winter, Summer)

result_df <- result_df%>%
  rename(SummerPrecipZscore = Summer, 
         WinterPrecipZscore = Winter)%>%
  mutate(PrecipCondition = case_when(
  WinterPrecipZscore < 0 & SummerPrecipZscore < 0 ~ "DD",
  WinterPrecipZscore < 0 & SummerPrecipZscore > 0 ~ "DW",
  WinterPrecipZscore > 0 & SummerPrecipZscore < 0 ~ "WD",
  WinterPrecipZscore > 0 & SummerPrecipZscore > 0 ~ "WW"))
