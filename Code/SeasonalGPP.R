#load in GPP 
#===============================================================================
#Load Necessary Packages 
#===============================================================================
library(lubridate)
library(dplyr)
library(tidyverse) 
#===============================================================================
#Load PrecipConditions.csv to Add Flux Output
#===============================================================================
PrecipCondititons <- read.csv("./Data/PrecipConditions.csv")

#===============================================================================
#Identify Date and GPP Columns in Flux Data
#===============================================================================
flux_data <- read.csv("./Data/FluxData/Regular/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = -9999, sep = ",", skip = 2)

flux_data$TIMESTAMP_START <- ymd_hm(flux_data$TIMESTAMP_START)

columns_of_interest <- flux_data %>% 
  mutate(Year = year(TIMESTAMP_START),
         Month = month(TIMESTAMP_START),
         Day = day(TIMESTAMP_START),
         Day_of_Year = yday(TIMESTAMP_START))%>%
  group_by(Year, Month, Day)%>%
  mutate(DailyAvgGPP = mean(GPP_PI, na.rm = T))%>% 
  select(Year, Month, Day, Day_of_Year, DailyAvgGPP)
columns_of_interest <- distinct(columns_of_interest)

#===============================================================================
#Label Water Years and Define Winter/Summer Seasons (Static Approach)
#===============================================================================
water_year <- columns_of_interest%>%
  mutate(water_year = ifelse(Month > 9, Year + 1, Year))

SeasonsDOY <- water_year%>%
  mutate(Season = case_when(Day_of_Year >=166 & Day_of_Year <= 288 ~ "Summer", Day_of_Year >=305 | Day_of_Year <= 59 ~ "Winter", T ~ "DrySeason"))

WinSumGPP <- SeasonsDOY%>%
  filter(Season != "DrySeason")%>%
  mutate(DailyTotalAvgCarbon = DailyAvgGPP*86400)
#===============================================================================
#Calculate Seasonal Z-Scores
#===============================================================================
AvgCarbon <- WinSumGPP%>%
  group_by(water_year, Season)%>%
  summarize(TotalAnnualC = sum(DailyTotalAvgCarbon, na.rm = T))%>%
  filter(TotalAnnualC != 0)

longterm_values <- AvgCarbon%>%
  group_by(Season)%>%
  summarize(longterm_avg = mean(TotalAnnualC, na.rm = T),
            longterm_sd = sd(TotalAnnualC, na.rm = T))
calc_df <- merge(AvgCarbon, longterm_values, by = "Season")
zscores <- calc_df%>%
  group_by(water_year, Season)%>%
  summarize(zscore = (TotalAnnualC - longterm_avg)/longterm_sd)%>%
  rename(WaterYR = water_year)
#===============================================================================
#Add Flux Z-Scores to PrecipConditions.csv
#===============================================================================
result_df <- zscores%>%
  pivot_wider(names_from = Season, 
              values_from = zscore)%>%
  select(WaterYR, Winter, Summer)%>%
  rename(WinterCarbonZscore = Winter,
         SummerCarbonZscore = Summer)

PrecipFlux <- merge(PrecipCondititons, result_df, by = "WaterYR")
PrecipFlux <- PrecipFlux%>%
  select(WaterYR, PrecipCondition, WinterPrecipZscore, WinterCarbonZscore, SummerPrecipZscore, SummerCarbonZscore)

write.csv(PrecipFlux, "./Data/PrecipFlux.csv")
