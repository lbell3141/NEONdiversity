#flux anomalies for amf tower records

PathToFluxData <- "./Data/FluxData/Regular"

site_csvs <- list.files(PathToFluxData, pattern = ".csv")
#paste flux data path in front of csv names instead of changing directory 
csv_dfs <- lapply(site_csvs, function(x) {
  df <- read.csv(file.path(PathToFluxData, x), na.strings = "-9999", skip = 2, sep = ",")
  
  #add site names
  siteID <- substr(x, 1, 10)
  df$siteID <- siteID
  return(df)
})

#single df for all sites
#all_flux <- do.call(rbind, csv_dfs)


#different lengths 
#different names
#individual selection and renaming