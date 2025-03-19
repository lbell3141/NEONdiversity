library(dplyr)
library(stringr)
library(ggplot2)
library(taxize)
library(tidyr)
library(data.table)

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

write.csv(woodherb_df, "./Data/SiteIGBPdomcov.csv", row.names = F)

#TRY explore data

PathToTRYdataSLA <- "./Data/TRY_PlantTraitDatabase/SLA/39636.txt"
trydatSLA <- read.table(PathToTRYdataSLA, sep = "\t", header = TRUE, fill = TRUE, quote = "")
PathToTRYdataLCC <- "./Data/TRY_PlantTraitDatabase/LCC/39686.txt"
trydatLCC <- read.table(PathToTRYdataLCC, sep = "\t", header = TRUE, fill = TRUE, quote = "")
PathToTRYdataGF <- "./Data/TRY_PlantTraitDatabase/GrowthForm/39729.txt"
trydatGF <- fread(PathToTRYdataGF, sep = "\t", encoding = "UTF-8", quote = "")

GF_datcoi <- trydatGF%>%
  filter(TraitID == 42,
         OrigValueStr != "",
         SpeciesName != "")%>%
  select(SpeciesName, OrigValueStr)%>%
  arrange(SpeciesName)%>%
  rename(Species = SpeciesName)

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
famLCC <- merge(coiunits, fam_df, by = "SpeciesName")%>%
  group_by(SpeciesName)%>%
  mutate(avgCC = mean(CarbonContent, na.rm = T),
            seCC = sd(CarbonContent, na.rm = TRUE) / sqrt(n()))%>%
  select(Family, SpeciesName, avgCC, seCC)%>%
  distinct()%>%
  rename(Species = SpeciesName)
#write.csv(famLCC, "./Data/TRY_PlantTraitDatabase/LCC/famLCC.csv")

LCC_GF <- merge(famLCC, GF_datcoi, by = "Species")%>%
  distinct()

choose_category <- function(values) {
  if (any(values %in% c("tree", "Tree", "T"))) {
    return("tree")
  } else if (any(values %in% c("shrub", "Shrub"))) {
    return("shrub")
  } else if (any(values %in% c("grass", "Grass", "herb", "Herb", "H"))) {
    return("grass")
  } else {
    return(NA)
  }
}

df_grouped <- LCC_GF %>%
  group_by(Species) %>%
  summarise(
    Family = first(Family),  
    avgCC = first(avgCC),  
    seCC = first(seCC),    
    OrigValueStr = choose_category(OrigValueStr)
  ) %>%
  filter(!is.na(OrigValueStr)) %>%
  ungroup()

write.csv(df_grouped, "./Data/TRY_PlantTraitDatabase/GrowthForm/GrowthForm.csv", row.names = F)
