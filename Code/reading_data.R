#using NEON packages to pull data

#load packages
library(neonUtilities)
library(neonOS)
library(terra)

PathtoTifs <- "./Data/NEON_lai-spectrometer-mosaic/NEON_lai-spectrometer-mosaic/NEON.D14.SRER.DP3.30012.001.2017-08.basic.20240705T211306Z.RELEASE-2024"
options(stringsAsFactors = F)
tif_files <- list.files(path = PathtoTifs, pattern = "\\.tif$", full.names = TRUE)
rasters <- stack(tif_files)

test_a <- rast("./Data/NEON_lai-spectrometer-mosaic/NEON_lai-spectrometer-mosaic/NEON.D14.SRER.DP3.30012.001.2017-08.basic.20240705T211306Z.RELEASE-2024/NEON_D14_SRER_DP3_506000_3524000_LAI.tif")
test_b <- rast("./Data/NEON_lai-spectrometer-mosaic/NEON_lai-spectrometer-mosaic/NEON.D14.SRER.DP3.30012.001.2017-08.basic.20240705T211306Z.RELEASE-2024/NEON_D14_SRER_DP3_502000_3524000_LAI.tif")
plot(test_b)
