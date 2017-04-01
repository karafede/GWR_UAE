
library(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(threadr)
library(gdalUtils)
library(rgdal)
library(raster)
library(RNetCDF)
library(readr)
library(gstat)
library(sp)
library(curl)
library(leaflet)
library(htmlwidgets)


  setwd("D:/website_MODIS")
  dir <- "D:/website_MODIS/UAE_boundary"
  ### shapefile for UAE
  shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  
  # ----- Transform to EPSG 4326 - WGS84 (required)
  shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
  # names(shp)
  
  shp_UAE@data$name <- 1:nrow(shp_UAE)
  # plot(shp_UAE)
  
  
  ################################################################################
  # subset data for a selected region
  # UAE
  

  # MODIS04_data <- read.csv(paste0("AOD_MOD04_10km_UAE","_",date,".csv"))[2:4]
  # DATA_EMIRATES <- subset(MODIS04_data, lon <= 80 & lon >= 10 & lat >= 10 & lat <= 55)
  # 
  # head(DATA_EMIRATES)
  
 # write.csv(DATA_EMIRATES, paste0("AOD_MOD04_10km_UAE","_", "006",".csv"))
 # DATA_EMIRATES <- read.csv(paste0("AOD_MOD04_10km_UAE","_","006" ,".csv"))[-1]
  DATA_EMIRATES <- read.csv(paste0("AOD_MOD04_10km_UAE","_","014" ,".csv"))[-1]
  
# converta data into a spatialpolygon dataframe------------------------------------
  
  DATA_EMIRATES$x <- DATA_EMIRATES$lon
  DATA_EMIRATES$y <- DATA_EMIRATES$lat
  
  coordinates(DATA_EMIRATES) = ~x + y  ## Set spatial coordinates to create a Spatial object:
  
  
  # make a variogram----------------------------------------------------------------
  
  vargram_AOD <- variogram(values ~ 1, DATA_EMIRATES) # calculates sample variogram values 
  
  # fite the variogram
  vargram_AOD_fit  <- fit.variogram(vargram_AOD, fit.ranges = FALSE, fit.sills = FALSE,
                                     vgm(1, c("Sph", "Exp", "Mat")), fit.kappa = TRUE)
  
  plot(vargram_AOD, vargram_AOD_fit) # plot the sample values, along with the fit model
  
  
  # make a regular empty grid
  x.range <- as.numeric(c(10, 85))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(-1.7, 60))  # min/max latitude of the interpolation area
  
  ## grid at 10km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                     y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
 
  plot(grd, cex = 1.5, col = "grey")
  points(DATA_EMIRATES, pch = 1, col = "red", cex = 1)
  
  f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
  dat.krg <- gstat::krige(values ~ 1, DATA_EMIRATES, grd, vargram_AOD_fit, nmax = 50)
  r <- raster(dat.krg)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  r <- crop(r, extent(shp_UAE))
  r <- mask(r, shp_UAE)
  plot(r)
  
  
  writeRaster(r, "AOD_MOD04_10km_UAE_014.tif", overwrite = TRUE)
  Data_Emirates_10km_tif <- raster(paste0("AOD_MOD04_10km_UAE_014.tif"))
  plot(Data_Emirates_10km_tif)

  

  ##########################################################################
  ##########################################################################
  
  # load Donkelaar Satellite-Derived PM2.5, 2015, at 35% RH [ug/m3] (with Geographical regression adjustment)
  
  PM25_2015_GRW <- raster("GlobalGWR_PM25_GL_201501_201512-RH35.nc")
  projection(PM25_2015_GRW) <- CRS("+proj=longlat +datum=WGS84")
  
  ### crop raster over the UAE shp file  ###############################
  PM25_2015_GRW <- crop(PM25_2015_GRW, extent(shp_UAE))
  PM25_2015_GRW <- mask(PM25_2015_GRW, shp_UAE)
  
  file.tiff_GWR_PM25_1km_2015 <- 'GWR_PM25_1km_2015.tif'
  GWR_PM25_1km_2015_tiff <- writeRaster(PM25_2015_GRW, filename = file.tiff_GWR_PM25_1km_2015, format = 'GTiff', overwrite = T)

  # check resoltuion of Donkelaar data and of ECMWF data
  res(GWR_PM25_1km_2015_tiff) # 0.01
  res(r) # 0.1
  
  ###################
  
  ## make resolution of MODIS-data as the one of GWR-------------------------------
  Data_Emirates_tif_1km = projectRaster(Data_Emirates_10km_tif, PM25_2015_GRW)
  Data_Emirates_tif_1km <- crop(Data_Emirates_tif_1km, extent(shp_UAE))
  Data_Emirates_tif_1km <- mask(Data_Emirates_tif_1km, shp_UAE)

  file.Emirates_tif_1km <- "AOD_MOD04_1km_UAE_014.tif"
  Emirates_tif_1km_tiff <- writeRaster(Data_Emirates_tif_1km, filename = file.Emirates_tif_1km, format = 'GTiff', overwrite = T)
  plot(Emirates_tif_1km_tiff)
  
  
  ###########################################################################
  #### make Leafleft map with current-AOD data 1km ###-----------------------
  
  library(leaflet)
  
 
  # define color palette
  rast_pal_EMIRATES <- colorNumeric(c("#9999FF", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),
                                    getValues(Emirates_tif_1km_tiff),na.color = "transparent")
  
  
  
  map <- leaflet() %>% 
    # setView(45, 25, 5) %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    addRasterImage(Emirates_tif_1km_tiff, 
                   colors = rast_pal_EMIRATES, 
                   opacity = 0.6,
                   group = "AOD_EMIRATES") %>%
    addLegend("bottomright", pal = rast_pal_EMIRATES, values = values(Emirates_tif_1km_tiff),# values = c(MIN_data, MAX_data),
              title = paste("<br><strong>AOD (MODIS-5km)","006","</strong>"),
              labFormat = labelFormat(prefix = ""),
              opacity = 0.6) %>%
    addPolygons(stroke = TRUE, data = shp_UAE,
                weight = 1.5, color = "black",
                fillOpacity = 0,
                group = "shape_UAE") %>%
    addLayersControl(
      baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
      overlayGroups = c("AOD_EMIRATES"),
      options = layersControlOptions(collapsed = TRUE)) 
  # map
  
  # save map
  saveWidget(map, paste0("AOD_Emirates","_","014",".html"), selfcontained = FALSE)

  
  
  
  
  
  
  
  