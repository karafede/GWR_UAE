library(sp)
library(raster)
library(dplyr)

ECMWF_var<-function(var_name, mon,yyyy){
 
  col_number<- which(var_name==c( "Date" , "Lon", "Lat", "AOD_MODIS","AOD_ECMWF" , "DUST_ECMWF", "SALT_ECMWF" ,"BC_ECMWF"  , "OM_ECMWF"  , "SO4_ECMWF" ))
  months_req<-sprintf("%02d",mon)
  year_req<-sprintf("%04d",yyyy)
  path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ECMWF_CAMS/", year_req, "/ECMWF_MODIS_csv_40km/****-", months_req, "-**.csv", sep = "")
  
  datafiles <- Sys.glob(path_name) #Or whatever identifies your files
  resultingStack <- stack()
  
  for(i in 1:NROW(datafiles)){
    csv_file <- read.csv(datafiles[1])
    name <- as.character(csv_file$Date[1])
    # checking if the PM25 is not there
    if(ncol(csv_file)> 10){
     all_variables<- c( "Date" , "Lon", "Lat", "AOD_MODIS","AOD_ECMWF" , "DUST_ECMWF", "SALT_ECMWF" ,"BC_ECMWF"  , "OM_ECMWF"  , "SO4_ECMWF" )
     csv_file<- csv_file[all_variables]
    }
    csv_file<- select(csv_file, c(Lon, Lat, col_number) )# make a variable for the stack u want
    coordinates(csv_file)<- ~ Lon + Lat
    gridded(csv_file) <- TRUE
    tempraster <- raster(csv_file )
    crs(tempraster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    names(tempraster)<-name
    resultingStack <- stack(resultingStack,tempraster)
  }
  
  return(resultingStack)
  
  # name_ras<-as.vector(names(resultingStack))
  # dawit<-(name_ras)
  # y <- grep('X2013.01.*', dawit)
  # jan_raster<-subset(resultingStack,y)
}




