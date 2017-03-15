
library(rgdal)
library(ggplot2)
library(gstat)
library(sp)
library(raster)   
library(plyr)
library(dplyr)
library(leaflet)
library(htmltools)
library(readr)
library(threadr)
library(htmlwidgets)
# library(ncdf4)
# library(RNetCDF)
# library(fields)
# library(rgeos)

### added
# library(maps)
# library(geoR)
# library(spatial)
# library(gstat)



# ###################################################################
# #########   Getting the monthly monitoring values           #######
# ###################################################################


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily filtered with 4 boxplot")
#setwd("D:/Daily filtered with 4 boxplot")
# setwd("disk3/fkaragulian/GWR/Daily filtered with 4 boxplot")


###########################################################################
###########################################################################


EAD_data_2013 <- read_csv("database_EAD_ 2013 _daily_filtered.csv")[3:14]
EAD_data_2014 <- read_csv("database_EAD_ 2014 _daily_filtered.csv")[3:14]
EAD_data_2015 <- read_csv("database_EAD_ 2015 _daily_filtered.csv")[3:14]
EAD_data_2016 <- read_csv("database_EAD_ 2016 _daily_filtered.csv")[3:14]

DM_data_2013 <- read_csv("database_DM_ 2013 _daily_filtered.csv")[3:14]
DM_data_2014 <- read_csv("database_DM_ 2014 _daily_filtered.csv")[3:14]
DM_data_2015 <- read_csv("database_DM_ 2015 _daily_filtered.csv")[3:14]
DM_data_2016 <- read_csv("database_DM_ 2016 _daily_filtered.csv")[3:14]

NCMS_data_2013 <- read_csv("database_NCMS_ 2013 _daily_filtered.csv")[3:14]
NCMS_data_2014 <- read_csv("database_NCMS_ 2014 _daily_filtered.csv")[3:14]
NCMS_data_2015 <- read_csv("database_NCMS_ 2015 _daily_filtered.csv")[3:14]
NCMS_data_2016 <- read_csv("database_NCMS_ 2016 _daily_filtered.csv")[3:14]

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)

str(AQ_data)

AQ_data[,1][AQ_data[,1]=="DUBAIAIRPORT"]<- "DUBAI AIR PORT"



AQ_data <- AQ_data %>%
  mutate(Date = ymd(Date, tz = "UTC"))%>%
  mutate(months=month(Date))%>%                # DG adding month
  mutate(years=year(Date))                     # DG adding year


AQ_data_PM25 <- AQ_data %>%
  filter(Pollutant == "PM2.5") %>%
  #filter(Date == as.Date("2016-08-26"))      # DG 
  filter (months==1)

# monthly mean of january
AQ_data_PM25 <- AQ_data_PM25 %>%
  group_by(Site, years) %>%
  summarize(mon_mean= mean(Daily_mean, na.rm = T))

# monthly mean of january
AQ_data_PM25 <- AQ_data_PM25 %>%
  group_by(Site) %>%
  summarize(sea_mean=mean(mon_mean, na.rm = T))

coordin_site<-filter(AQ_data, Date==as.Date("2013-01-01") & Pollutant == "PM2.5" )


AQ_data_PM25<- left_join(AQ_data_PM25, coordin_site, by= c("Site"= "Site" ))


AQ_data_PM25 <- AQ_data_PM25 %>%
  select(Longitude,
         Latitude,
         sea_mean)

# remove all lines with NA
AQ_data_PM25 <- na.omit(AQ_data_PM25)



# ###################################################################
# #########   kRIGING interpolation of the monitoring values  #######
# ###################################################################

# shapefile of UAE for the kriging

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)

### USING THE FUNCTION kriging_func.R
source("D:/Air Quality/GWR/kriging_func.R")
###    kriging_points <- function(dawit, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")

resl_ras= 0.1

r_moni <-kriging_points(dawit=AQ_data_PM25, resl_ras, shp_UAE = "D:/Air Quality/GWR/UAE_boundary"  )
str(r_moni)
#"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

writeRaster(r_moni, paste0("D:/Air Quality/GWR/Saves from GWR script/in_situ_kriging_UAE.tif"), overwrite = TRUE)
rm(list = ls(all = TRUE))


######################################################################


# ###################################################################
# #########   Averaging the monthly AOD values from MODIS     #######
# ###################################################################

### for the year of 2016

year_req<-sprintf("%04d-",2016)
months_req<-sprintf("%02d",01)

path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2016_MODIS_processed/2016_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")

datafiles <- Sys.glob(path_name) #Or whatever identifies your files
resultingStack <- stack()
for(i in 1:NROW(datafiles)){
  tempraster <- raster(datafiles[i])
  resultingStack <- stack(resultingStack,tempraster)
}

x <- reclassify(resultingStack, cbind(0, NA))
r_AOD_2016 <- mean(x, na.rm=TRUE)*94
r_AOD_2016<- projectRaster(r_AOD_2016, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
str(r_AOD_2016)
plot(r_AOD_2016)
res(r_AOD_2016)

### for the year of 2015

year_req<-sprintf("%04d-",2015)
months_req<-sprintf("%02d",01)

path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2015_MODIS_processed/2015_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")

datafiles <- Sys.glob(path_name) #Or whatever identifies your files
resultingStack <- stack()
for(i in 1:NROW(datafiles)){
  tempraster <- raster(datafiles[i])
  resultingStack <- stack(resultingStack,tempraster)
}

x <- reclassify(resultingStack, cbind(0, NA))
r_AOD_2015 <- mean(x, na.rm=TRUE)*94
r_AOD_2015<- projectRaster(r_AOD_2015, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
str(r_AOD_2015)
plot(r_AOD_2015)
res(r_AOD_2015)


### for the year of 2014

year_req<-sprintf("%04d-",2014)
months_req<-sprintf("%02d",01)

path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2014_MODIS_processed/2014_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")

datafiles <- Sys.glob(path_name) #Or whatever identifies your files
resultingStack <- stack()
for(i in 1:NROW(datafiles)){
  tempraster <- raster(datafiles[i])
  resultingStack <- stack(resultingStack,tempraster)
}

x <- reclassify(resultingStack, cbind(0, NA))
r_AOD_2014 <- mean(x, na.rm=TRUE)*94
r_AOD_2014<- projectRaster(r_AOD_2014, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
str(r_AOD_2014)
plot(r_AOD_2014)
res(r_AOD_2014)


### for the year of 2013

year_req<-sprintf("%04d-",2013)
months_req<-sprintf("%02d",01)

path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2013_MODIS_processed/2013_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")

datafiles <- Sys.glob(path_name) #Or whatever identifies your files
resultingStack <- stack()
for(i in 1:NROW(datafiles)){
  tempraster <- raster(datafiles[i])
  resultingStack <- stack(resultingStack,tempraster)
}

x <- reclassify(resultingStack, cbind(0, NA))
r_AOD_2013 <- mean(x, na.rm=TRUE)*94
r_AOD_2013<- projectRaster(r_AOD_2013, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
str(r_AOD_2013)
plot(r_AOD_2013)
res(r_AOD_2013)


### averaging all the years

AOD_mean_jan <- overlay(r_AOD_2016, r_AOD_2015, r_AOD_2014, r_AOD_2013, fun= mean)
plot(AOD_mean_jan)

writeRaster(AOD_mean_jan, 'D:/Air Quality/GWR/Saves from GWR script/AOD_mean_jan.tif',overwrite=TRUE)

rm(list = ls(all = TRUE))




# ###################################################################
# #########   Land Use layer to be used for the Regression    #######
# ###################################################################

# load raster land use UAE
##### Lancover == 16  ---> desert area #########

LU <- raster("D:/Air Quality/GWR/AD_DUBAI_Modis.tif")
plot(LU)

LU[LU < 16] <- 0
LU[LU > 16] <- 0
LU <- LU/16
plot(LU)
resampled_LU<-aggregate(LU,fact=100, fun= sum ) # changing fact number to change the aggregation of the pixels
LU_fract_desert <- resampled_LU/10000

str(LU_fract_desert)
res(LU_fract_desert)
plot(LU_fract_desert)

##### Lancover == 13  ---> urban area #########

LU <- raster("D:/Air Quality/GWR/AD_DUBAI_Modis.tif")

plot(LU)

LU[LU < 13] <- 0
LU[LU > 13] <- 0
LU <- LU/13
plot(LU)
resampled_LU<-aggregate(LU,fact=100, fun= sum ) # changing fact number 
LU_fract_urban <- resampled_LU/10000

plot(LU_fract_urban)

res(LU_fract_urban)


####### exporting the landuse layers

writeRaster(LU_fract_urban, "D:/Air Quality/GWR/Saves from GWR script/urban_fraction.tif",overwrite=TRUE)
writeRaster(LU_fract_desert, "D:/Air Quality/GWR/Saves from GWR script/desert_fraction.tif",overwrite=TRUE)

rm(list = ls(all = TRUE))





# ###################################################################
# #########       Geographic Weighted Regression Model        #######
# ###################################################################

# Importing and restructuring the rasters

setwd("D:/Air Quality/GWR/Saves from GWR script/")

LU_fract_desert<- raster("desert_fraction.tif")
LU_fract_urban<- raster("urban_fraction.tif")
AOD_mean_jan<-raster("AOD_mean_jan.tif")
r_moni<- raster("in_situ_kriging_UAE.tif")

plot(LU_fract_urban)



LU__urban10km <- resample(LU_fract_urban,r_moni,"bilinear")
LU__desert10km <- resample(LU_fract_desert,r_moni,"bilinear")
r_AOD_sampled <- resample(AOD_mean_jan,r_moni,"bilinear")

plot(r_AOD_sampled)

LU__urban10km <- mask(LU__urban10km, r_moni)
LU__desert10km <- mask(LU__desert10km, r_moni)
r_AOD_sampled <- mask(r_AOD_sampled, r_moni)


# diff between in situ data and PM25_AOD
BIAS <- r_moni-r_AOD_sampled


#######################################################
##### changing the layers to points    Method I  ######
#######################################################
# plot(BIAS)
# BIAS_pts <- rasterToPoints(BIAS)
# LU_desert_pts <- rasterToPoints(LU__desert10km)
# LU_urban_pts <- rasterToPoints(LU__urban10km)
# AOD_pts <- rasterToPoints(r_AOD_sampled)
# 
# 
# mydata <- cbind(BIAS_pts[,1],
#                 BIAS_pts[,2],
#                 BIAS_pts[,3],
#                 LU_desert_pts[,3],
#                 LU_urban_pts[,3])
# mydata<-as.data.frame(mydata)
# 
# colnames(mydata) <- c("Lon", "Lat", "BIAS", "desert", "urban")
# library(spgwr)
# 
# bwG <- gwr.sel(BIAS ~ desert ,#+ urban ,
#                data= mydata, coords = cbind( mydata$Lon , mydata$Lat), gweight = gwr.Gauss,
#                verbose = FALSE)
# 
# gwrG <- gwr(BIAS ~ desert + urban ,
#             data= mydata, coords = cbind( mydata$Lon , mydata$Lat), bandwidth = bwG,
#             gweight = gwr.Gauss, hatmatrix = TRUE)


#######################################################
##### as SpatialPolygonsDataFrame layers Method II ####
#######################################################


library(spgwr)
BIAS <- r_moni-r_AOD_sampled

BIAS <- as(BIAS, 'SpatialPolygonsDataFrame')
LU__desert10km <- as(LU__desert10km, 'SpatialPolygonsDataFrame')
LU__urban10km <- as(LU__urban10km, 'SpatialPolygonsDataFrame')
r_AOD_sampled <- as(r_AOD_sampled, 'SpatialPolygonsDataFrame')


combined_data<-cbind(BIAS,LU__desert10km,LU__urban10km,r_AOD_sampled)

combined_data$desert_fraction
combined_data$layer
combined_data$urban_fraction

bwG <- gwr.sel(layer ~ urban_fraction + desert_fraction ,
               data= combined_data,  gweight = gwr.Gauss,
               verbose = FALSE)
gwrG <- gwr(layer ~ urban_fraction + desert_fraction  ,
            data= combined_data,  bandwidth = bwG,
            gweight = gwr.Gauss, hatmatrix = TRUE)





BIAS_sp <- as(BIAS, 'SpatialPointsDataFrame')
LU__desert10km_sp <- as(LU__desert10km, 'SpatialPointsDataFrame')
LU__urban10km_sp <- as(LU__urban10km, 'SpatialPointsDataFrame')
r_AOD_sampled_sp <- as(r_AOD_sampled, 'SpatialPointsDataFrame')

combined_data_pnt<-cbind(BIAS_sp,LU__desert10km_sp,LU__urban10km_sp,r_AOD_sampled_sp)

combined_data_pnt$

bwG_pnt <- gwr.sel(layer ~ urban_fraction,# + desert_fraction ,
                 data= combined_data_pnt,  gweight = gwr.Gauss,
                 verbose = FALSE)
gwrG_pnt <- gwr(layer ~ urban_fraction,# + desert_fraction  ,
            data= combined_data_pnt,  bandwidth = bwG_pnt,
            gweight = gwr.Gauss, hatmatrix = TRUE)



combined_data_pnt@coords

remove(result)
gwrG_pnt$SDF$

result<- as.data.frame(gwrG_pnt$SDF )

result<-cbind(result,combined_data_pnt@coords)

##### ploting the cofficients 
### urban

coffi_urban_raster<-select(result,x,y,urban_fraction)
coordinates(coffi_urban_raster) <- ~ x + y
gridded(coffi_urban_raster) <- TRUE
coffi_urban_raster <- raster(coffi_urban_raster)
plot(coffi_urban_raster)

### desert

coffi_desert_raster<-select(result,desert_fraction,x,y)
coordinates(coffi_desert_raster) <- ~ x + y
gridded(coffi_desert_raster) <- TRUE
coffi_desert_raster <- raster(coffi_desert_raster)
plot(coffi_desert_raster)

### Intercept

coffi_intercept_raster<-select(result,X.Intercept.,x,y)
coordinates(coffi_intercept_raster) <- ~ x + y
gridded(coffi_intercept_raster) <- TRUE
coffi_intercept_raster <- raster(coffi_intercept_raster)
plot(coffi_intercept_raster)


######### results

estimated_BIAS <- coffi_urban_raster*LU__urban10km + coffi_intercept_raster + r_AOD_sampled # + coffi_desert_raster*LU__desert10km 


plot(estimated_BIAS)
plot(r_AOD_sampled)

coeffi_GWR<-result$desert_fraction
coeffi_GWR<-as.data.frame(coeffi_GWR)
coeffi_GWR$urdan<-result$urban_fraction
coeffi_GWR$intercept<-result$X.Intercept.

dawit <- as(coeffi_GWR$urdan, 'SpatialPolygonsDataFrame')

str(r_AOD_sampled)

hist(daw_cor$residuals, nclass=100)


fed<-daw_cor$effects

# write.csv(fed, file="dawit.csv")


str(fed)


dawi<-as.data.frame(gwrG$SDF)
dawi$test<-abs(dawi$urban_fraction)-2*dawi$urban_fraction_se


daw_cor<-gwrG$lm

pred<-raster(gwrG$localR2)

pred<- rasterFromXYZ(as.data.frame(cbind(daw_cor,dawi$pred)), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

dawi<-gwrG$SDF@data
daw_cor<-gwrG$SDF@coords

hist(dawi$localR2, nclass=100)

pred<- rasterFromXYZ(as.data.frame(cbind(daw_cor,dawi$pred)), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(pred)
ex<-extent(LU__desert10km)
pred<- setExtent(pred,ex)
outputRaster <- overlay(pred, LU__desert10km, fun=function(r1, r2){return(r1*r2)})
last<- pred*LU__desert10km


plot(pred)
res(LU__desert10km)
extent(pred)

LU_desert_pts <- rasterToPoints(LU__desert10km)
prep_da <- rasterToPoints(pred)
AOD_pts <- rasterToPoints(r_AOD_sampled)

pred<-rasterize(gwrG$SDF$desert)



dawi@coords

write.csv(gwr100$SDF, file = "GWR100_AOD_LU_DSR_URB_1km.csv")

GWR <- read.csv("GWR100_AOD_LU_DSR_URB_1km.csv",header = TRUE)

GWR$In_situ <- mydata$PM25_in_situ
GWR$AOD_PM25 <- mydata$AOD_PM25
GWR$DSR <- (mydata$desert)*(GWR$desert)  
GWR$URB <- (mydata$urban)*(GWR$urban)
GWR$LU12 <- (mydata$LU_12)*(GWR$LU_12)  
GWR$LU7 <- (mydata$LU_7)*(GWR$LU_7)


df <- data.frame(GWR$AOD_PM25, GWR$DSR, GWR$URB, GWR$LU12, GWR$LU7)

ADJ_AOD_PM25 <- rowSums(df, na.rm=T)
GWR_AOD_PM25 <- cbind(GWR, ADJ_AOD_PM25)

write.csv(GWR_AOD_PM25, file = "GWR_AOD_PM25_UAE.csv")

GWR_AOD_PM25 <- read_csv("GWR_AOD_PM25_UAE.csv")

ADJ_AOD_PM25 <- GWR_AOD_PM25 %>%
  select(coord.x,
         coord.y,
         ADJ_AOD_PM25)
colnames(ADJ_AOD_PM25) <- c("Lon", "Lat", "ADJ_AOD_PM25")
ADJ_AOD_PM25 <- ADJ_AOD_PM25 %>%
  filter(ADJ_AOD_PM25 > 0)

# make a new raster with PM25

coordinates(ADJ_AOD_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(ADJ_AOD_PM25) <- TRUE
r <- raster(ADJ_AOD_PM25)
projection(r) <- CRS("+proj=longlat +datum=WGS84")

r <- crop(r, extent(shp_UAE))
r <- mask(r, shp_UAE)
plot(r)

writeRaster(r, paste0("ADJ_AOD_PM25.tif"), overwrite = TRUE)
PM25_AOD_ADJ_tiff <- raster("ADJ_AOD_PM25.tif")


plot(r)
g <- as(r, 'SpatialGridDataFrame')





####### Map  all layers data with Leaflet #############################################

# define color palette

min_in_situ = minValue(in_situ_IDW_UAE)
min_in_situ
max_in_situ = maxValue(in_situ_IDW_UAE)
max_in_situ

min_PM25_AOD = minValue(PM25_AOD_tiff)
min_PM25_AOD
max_PM25_AOD = maxValue(PM25_AOD_tiff)
max_PM25_AOD

min_PM25_ADJ_AOD = minValue(PM25_AOD_ADJ_tiff)
min_PM25_ADJ_AOD
max_PM25_ADJ_AOD = maxValue(PM25_AOD_ADJ_tiff)
max_PM25_ADJ_AOD

MIN_PAL <- 3.5
MAX_PAL <- 70.5

# pal_PM25_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                              getValues(in_situ_IDW_UAE),na.color = "transparent")
# 
# pal_PM25_AOD_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                                  getValues(PM25_AOD_tiff),na.color = "transparent")
# 
# pal_PM25_AOD_ADJ_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                                  getValues(PM25_AOD_ADJ_tiff),na.color = "transparent")

pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal_LU_1km <- colorNumeric(c("#0000ff", "#ffff00", "#ff0000"),
                           getValues(LU_1km),na.color = "transparent")

map <- leaflet() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(in_situ_IDW_UAE, colors = pal, opacity = 0.5,
                 group = "in situ PM25") %>%
  addRasterImage(PM25_AOD_tiff, colors = pal, opacity = 0.5,
                 group = "PM25 AOD 1km") %>%
  addRasterImage(PM25_AOD_ADJ_tiff, colors = pal, opacity = 0.5,
                 group = "PM25 AOD ADJ 1km") %>%
  addRasterImage(LU_1km, colors = pal_LU_1km, opacity = 0.5,
                 group = "LU 1km") %>%
  addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
            title = "<br><strong>PM25</strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  
map

# save map
saveWidget(map, paste0("ADJ_AOD_PM25_LU.html"), selfcontained = FALSE)