
library(raster)
library(rgdal)

setwd("C:/MASDAR_FK/Air Quality/DEM_UAE")

UAE_DEM <- raster("UAE_ASTR_dem30m.tif")
plot(UAE_DEM)


dir <- "C:/MASDAR_FK/Air Quality/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
# plot(shp_UAE)

# crop DEM raster
UAE_DEM <- crop(UAE_DEM, extent(shp_UAE))
UAE_DEM <- mask(UAE_DEM, shp_UAE)
plot(UAE_DEM)

writeRaster(UAE_DEM, "DEM_UAE.tif", overwrite = TRUE)
