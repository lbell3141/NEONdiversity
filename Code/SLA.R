library(dplyr)
library(stringr)
library(ggplot2)
library(taxize)
library(tidyr)

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
    IGBP %in% c("WSA", "OSH") ~ "Mixed",
    IGBP %in% c("DBF", "DNF", "EBF", "ENF", "MF", "CSH") ~ "Wood Dominated",
    T ~ "Unclassified"))%>%
  mutate(Group = factor(Group, levels = c("Herb Dominated", "Mixed", "Wood Dominated", "Unclassified")))
ggplot(woodherb_df, aes(x = factor(Group)))+
  geom_bar(fill = "black")+
  labs(x = "Group", y = "Number of Sites")+
  theme_minimal()

#attempt to obtain dominant species cover
veg_dat <- badm %>%
  filter(grepl("SPP", VARIABLE_GROUP))



#TRY explore data

PathToTRYdataSLA <- "./Data/TRY_PlantTraitDatabase_SLA/39636.txt"
trydatSLA <- read.table(PathToTRYdataSLA, sep = "\t", header = TRUE, fill = TRUE, quote = "")

PathToTRYdataLCC <- "./Data/TRY_PlantTraitDatabase/LCC/39686.txt"
trydatLCC <- read.table(PathToTRYdataLCC, sep = "\t", header = TRUE, fill = TRUE, quote = "")

try_coi <- trydatLCC%>%
  filter(TraitID == 570,
         OrigValueStr != "")%>%
  select(SpeciesName, OrigValueStr, OrigUnitStr)%>%
  arrange(SpeciesName)

coiunits <- try_coi %>%
  mutate(OrigValueStr = as.numeric(OrigValueStr))%>%
  mutate(OrigValueStr = ifelse(OrigUnitStr == "m2/kg(C)", (1 / OrigValueStr) * 1000, OrigValueStr),
         OrigUnitStr = case_when(OrigUnitStr %in% c("g/m2", "g m-2", "m2/kg(C)") ~ "g/m2"))%>%
  rename(Unit = OrigUnitStr,
         CarbonContent = OrigValueStr)

#pull family names
sp_names <- coiunits %>%
  select(SpeciesName) %>%
  distinct()%>%
  mutate(SpeciesName = str_replace_all(SpeciesName, "[^[:alnum:] -]", " "))
sp_list <- coiunits %>%
  select(SpeciesName) %>%
  distinct() %>%
  mutate(SpeciesName = str_replace_all(SpeciesName, "[^[:alnum:] -]", " ")) %>%
  separate(SpeciesName, into = c("Genus", "Species"), sep = " ", extra = "merge")%>% 
  filter(!is.na(Genus))
sp_list <- cbind(sp_names, sp_list)

fam_df <- sp_list %>%
  mutate(Family = sapply(SpeciesName, function(x) {
    result <- tax_name(x, get = "family", db = "ncbi")
    family_name <- result$family
    return(family_name)
  })) %>%
  select(SpeciesName, Family)

taxLCC <- merge(coiunits, fam_df, by = "SpeciesName")%>%
  group_by(Family)%>%
  summarize(avgCC = mean(CarbonContent, na.rm = T),
         seCC = sd(CarbonContent, na.rm = TRUE) / sqrt(n()))
