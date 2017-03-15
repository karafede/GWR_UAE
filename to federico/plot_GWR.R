
library(raster)
library(leaflet)
library(htmlwidgets)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico")

# load raster for in situ data
PM25_in_situ_January <- raster("in_situ_kriging_UAE_1KM.tif")
plot(PM25_in_situ_January)
res(PM25_in_situ_January)

# load raster for satellite data converted to PM2.5
PM25_Sat_January <- raster("AOD_mean_jan_1km.tif")
plot(PM25_Sat_January)
res(URBAN_UAE)


# load raster for urban fraction 
URBAN_UAE <- raster("urban_fraction.tif")
plot(URBAN_UAE)


# load raster for desert fraction
DESERT_UAE <- raster("desert_fraction.tif")
plot(DESERT_UAE)


#load raster for adjusted PM2.5
ADJ_PM25_SATELLITE <- raster("adjusted_Sat_pm25_1km.tif")
plot(ADJ_PM25_SATELLITE)



########################################################################
### plots ##############################################################

MIN_PAL <- 20
MAX_PAL <- 62

pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal_LU_URB <- colorNumeric(c("#0000ff", "#ffff00", "#ff0000"),
                           getValues(URBAN_UAE),na.color = "transparent")

pal_LU_DESERT <- colorNumeric(c("#0000ff", "#ffff00", "#ff0000"),
                           getValues(DESERT_UAE),na.color = "transparent")


map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(PM25_in_situ_January, colors = pal, opacity = 0.5,
                 group = "in situ PM25") %>%
  addRasterImage(PM25_Sat_January, colors = pal, opacity = 0.5,
                 group = "PM25 AOD 1km") %>%
  addRasterImage(URBAN_UAE, colors = pal_LU_URB, opacity = 0.5,
                 group = "URBAN_UAE") %>%
  addRasterImage(DESERT_UAE, colors = pal_LU_DESERT, opacity = 0.5,
                 group = "DESERT_UAE") %>%
  addRasterImage(ADJ_PM25_SATELLITE, colors = pal, opacity = 0.5,
                 group = "PM25 AOD ADJ 1km") %>%
  addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
            title = "<br><strong>PM25</strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("in situ PM25", "PM25 AOD 1km", "PM25 AOD ADJ 1km", "URBAN_UAE", "DESERT_UAE"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("URBAN_UAE", "in situ PM25","PM25 AOD 1km", "DESERT_UAE")) 

map



# save map
saveWidget(map, paste0("ADJ_AOD_PM25_LU.html"), selfcontained = FALSE)
