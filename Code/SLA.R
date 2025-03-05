library(dplyr)
library(ggplot2)
#IGBP reclass into herb, woody, mixed

IGBPmetadata <- "./Data/AMF_AA-Net_BIF_CCBY4_20250201.csv"
badm <- read.csv(IGBPmetadata)
IGBP_df <- read.csv(IGBPmetadata)%>%
  filter(VARIABLE == "IGBP")%>%
  select(-c(VARIABLE_GROUP, VARIABLE, GROUP_ID))%>%
  rename(SiteID = SITE_ID,
         IGBP = DATAVALUE)
woodherb_df <- IGBP_df %>%
  mutate(Group = case_when(
    IGBP %in% c("GRA", "SAV", "WET") ~ "Herb Dominated",
    IGBP %in% c("WSA", "OSH", "CVM") ~ "Mixed",
    IGBP %in% c("DBF", "DNF", "EBF", "ENF", "MF", "CSH") ~ "Wood Dominated",
    T ~ "Unclassified"))%>%
  mutate(Group = factor(Group, levels = c("Herb Dominated", "Mixed", "Wood Dominated", "Unclassified")))
ggplot(woodherb_df, aes(x = factor(Group)))+
  geom_bar(fill = "black")+
  labs(x = "Group", y = "Number of Sites")+
  theme_minimal()
#TRY explore data

PathToTRYdata <- "./Data/TRY_PlantTraitDatabase_SLA/39636.txt"

trydat <- read.table(PathToTRYdata, sep = "\t", header = TRUE, fill = TRUE, quote = "")
