library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)

kriging_points <- function(dawit, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary"){
  
  #masking layer or shapefile
  
  if (is.character(shp_UAE)) {
    setwd(shp_UAE)
    dir<- shp_UAE
    shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
    
    # ----- Transform to EPSG 4326 - WGS84 (required)
    shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
  }

  # 
  # dawit$x <- dawit$Longitude
  # dawit$y <- dawit$Latitude
  # 
  # coordinates(dawit) = ~ x + y  ## Set spatial coordinates to create a Spatial object:
  # 
  ## make a variogram----------------------------------------------------------------
  
  limit_x_y<-extent(shp_UAE)
  shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  ras_points<-rasterToPoints(dawit, fun=NULL, spatial=T)

  
  vargram_PM251 <- variogram(layer ~ 1, ras_points) # calculates sample variogram values
  nn<-floor(length(vargram_PM251$gamma)/2)
  var_for_fit<- mean(vargram_PM251[nn:nrow(vargram_PM251),3])
  
  
  # fit the variogram
  vargram_PM251_fit  <- fit.variogram(vargram_PM251, fit.ranges = FALSE, fit.sills = FALSE,
                                      vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
  
  plot(vargram_PM251, vargram_PM251_fit) # plot the sample values, along with the fit model
  
  plot(vargram_PM251_fit)
  
  # make a regular empty grid
  x.range <- as.numeric(c(floor(limit_x_y[1]-1),ceiling(limit_x_y[2]+1)))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(floor(limit_x_y[3]-1),ceiling(limit_x_y[4]+1)))  # min/max latitude of the interpolation area
  
  
  ## grid at 5km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  crs(grd)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  plot(grd, cex = 1.5, col = "grey")
  points(ras_points, pch = 1, col = "red", cex = 1)
  
  
  #f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
  dat.krg <- gstat::krige(layer ~ 1, ras_points, grd, vargram_PM251_fit, nmax = 50)
  r <- raster(dat.krg)
  r <- crop(r, extent(shp_UAE))
  r <- mask(r, shp_UAE)
  
  plot(r)
  
  return(r)
  
}

########################################################################################################
###  david_re <- kriging_points(dawit, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary") #####
########################################################################################################
