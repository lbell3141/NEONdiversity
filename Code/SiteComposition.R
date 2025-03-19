#get site species list from BADM

library(dplyr)
library(tidyr)

PathToBADM <- "./Data/AMF_AA-Net_BIF_CCBY4_20250201.csv"
PathtoNRCScodes <- "./Data/NRCS_PlantCodes.csv"
badm <- read.csv(PathToBADM)
PlantCodes <- read.csv(PathtoNRCScodes, skip = 1, header = T)

sp_df <- badm%>%
  filter(VARIABLE %in% c("SPP_O", "SPP_O_PERC", "SPP_COMMENT"))

piv_dat <- sp_df %>%
  filter(VARIABLE %in% c("SPP_O", "SPP_O_PERC", "SPP_COMMENT")) %>%
  pivot_wider(
    names_from = VARIABLE,
    values_from = DATAVALUE,
  )%>%
  rename(
    SiteID = SITE_ID,
    GroupID = GROUP_ID,
    Species = SPP_O,
    PercentCover = SPP_O_PERC,
    Comment = SPP_COMMENT
  )

nrcs_col <- piv_dat %>%
  mutate(
    NRCS_ID = if_else(grepl("\\(NRCS plant code\\)", Species), 
                      gsub("\\(NRCS plant code\\)", "", Species) %>% trimws(), 
                      NA_character_),
    Species = if_else(grepl("\\(NRCS plant code\\)", Species), NA_character_, Species)
  )
PlantCodes <- PlantCodes %>%
  select(Accepted.Symbol, Scientific.Name)%>%
  rename(NRCS_ID = Accepted.Symbol, Species_lookup = Scientific.Name)
nrcs_col <- left_join(nrcs_col, PlantCodes, by = "NRCS_ID") %>%
  mutate(Species = if_else(is.na(Species), Species_lookup, Species)) %>%
  select(-Species_lookup)


#add study classifications
classdf <- merge(nrcs_col, woodherb_df, by = "SiteID")%>%
  filter(Group != "Unclassified")
#first LCC site data check:
LCC_siteSp <- merge(classdf, famLCC, by = "Species")





