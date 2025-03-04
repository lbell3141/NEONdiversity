#TRY explore data

PathToTRYdata <- "./Data/TRY_PlantTraitDatabase_SLA/39636.txt"
trydat <- read.table(PathToTRYdata, sep = '\t',header = T)

trydat <- read.table(PathToTRYdata, sep = "\t", header = TRUE, fill = TRUE, quote = "")
