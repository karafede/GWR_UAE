library(RColorBrewer)
runApp(appDir = "D:/shiny_AQ/AOD_web", host="0.0.0.0" , port= 10)






# diff between in situ data and PM25_AOD
BIAS <- r_moni-r_AOD_sampled

save.image("dawit.RData")


dawit<-as.data.frame(gwrG_pnt$SDF)

Adding_SO4_r2 <-rasterFromXYZ(dawit[, c("x", "y", "localR2")], crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
Adding_SO4_pred<-rasterFromXYZ(dawit[, c("x", "y", "pred")] ,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
Adding_SO4_pred_se<-rasterFromXYZ(dawit[, c("x", "y", "pred.se")], crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

plot(Adding_SO4_pred)
hist(Adding_SO4_pred,breaks=100)



cuts=c(-6,-4,-2,0,2,4,6) #set breaks

pal <- colorRampPalette(c("blue","red"))
plot(Adding_SO4_pred, breaks=cuts, col = pal(7))

hist(Adding_SO4_pred, breaks=100)

dada<- crop(r_AOD_sampled, Adding_SO4_pred)

plot(Adding_SO4_r2, useRaster=T)

cuts=c(100,150,160,170,180,190,200) #set breaks
pal <- colorRampPalette(c("white","black"))

plot(r_moni)
plot(r_AOD_sampled)
+Adding_SO4_pred)
plot(Adding_SO4_pred)


test<- as.data.frame(combined_data_pnt@data)

test_bais<-as.data.frame(combined_data_pnt@data$BIAS)

cor(cbind(test_bais,test))

r = raster(volcano) #raster object


####monitroing- predicted

moni_adjusted_exte<- crop(r_moni, Adding_SO4_pred)
corrected_sat<- moni_adjusted_exte-Adding_SO4_pred
plot(r_AOD_sampled)
hist(r_AOD_sampled, breaks=100)
hist(corrected_sat, breaks=100)

dawit_plot("LU_ED",corrected_sat,Adding_SO4_r2)


dawit_plot<- function(filename="LU_", corrected_sat, Adding_SO4_r2){

  file<-paste(filename,"_map.png", sep = "")
  png(file)
  plot(corrected_sat)
  dev.off()
  file<-paste(filename,"_dis.png", sep = "")
  png(file)
  hist(corrected_sat, breaks=100)
  dev.off()
  file<-paste(filename,"_r2.png", sep = "")
  png(file)
  hist(Adding_SO4_r2, breaks=100)
  dev.off()
}

setwd("D:/Air Quality/GWR/Saves from GWR script/")

ECMWF_OM_ECMWF_jan_10km_test<- raster("ECMWF_OM_ECMWF_jan_10km.tif")*94
ECMWF_BC_ECMWF_jan_10km_test<- raster("ECMWF_BC_ECMWF_jan_10km.tif")*94
ECMWF_SALT_ECMWF_jan_10km_test<- raster("ECMWF_SALT_ECMWF_jan_10km.tif")*94
ECMWF_DUST_ECMWF_jan_10km_test<- raster("ECMWF_DUST_ECMWF_jan_10km.tif")*94
ECMWF_SO4_jan_10km_test<- raster("ECMWF_SO4_jan_10km.tif")*94

plot(ECMWF_SO4_jan_10km_test)
plot(ECMWF_DUST_ECMWF_jan_10km_test)



plot(Adding_SO4_r2)
str(Adding_SO4)




###### exporting @ 1km resolution

###### Input report as a tif file

source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")

ex_OM_ECMWF_1km<- kriging_points(ECMWF_OM_ECMWF_jan_10km, resl_ras= 0.01)
plot(ex_OM_ECMWF_1km)
writeRaster( ex_OM_ECMWF_1km, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/OM_raster_1km.tif",overwrite=TRUE)

ex_BC_ECMWF_1km<- kriging_points(ECMWF_BC_ECMWF_jan_10km, resl_ras= 0.01)
plot(ex_BC_ECMWF_1km)
writeRaster( ex_BC_ECMWF_1km, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/BC_raster_1km.tif",overwrite=TRUE)

ex_ED_ECMWF_1km<- kriging_points(ED, resl_ras= 0.01)
plot(ex_ED_ECMWF_1km)
writeRaster( ex_ED_ECMWF_1km, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/ED_raster_1km.tif",overwrite=TRUE)

ex_DUST_ECMWF_1km<- kriging_points(ECMWF_DUST_ECMWF_jan_10km, resl_ras= 0.01)
plot(ex_DUST_ECMWF_1km)
writeRaster( ex_DUST_ECMWF_1km,"Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/DUST_raster_1km.tif",overwrite=TRUE)

ex_SO4_jan_1km<- kriging_points(ECMWF_SO4_jan_10km, resl_ras= 0.01)
plot(ex_SO4_jan_1km)
writeRaster(ex_SO4_jan_1km, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/SO4_ECMWF_1km.tif",overwrite=TRUE)


#### results report as a tif file

dawit<-as.data.frame(gwrG_pnt$SDF)

Output_r2 <-rasterFromXYZ(dawit[, c("x", "y", "localR2")], crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
Output_pred<-rasterFromXYZ(dawit[, c("x", "y", "pred")] ,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
Output_pred_se<-rasterFromXYZ(dawit[, c("x", "y", "pred.se")], crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")


#### monitroing- predicted

moni_adjusted_exte<- crop(r_moni, Output_pred)
corrected_sat<- moni_adjusted_exte-Output_pred

#####

BIAS_predicted<- kriging_points(Output_pred, resl_ras= 0.01)
plot(BIAS_predicted)
writeRaster(BIAS_predicted, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/BIAS_predicted.tif",overwrite=TRUE)


corrected_sat_1KM<- kriging_points(corrected_sat, resl_ras= 0.01)
plot(corrected_sat_1KM)
writeRaster(corrected_sat_1KM, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/corrected_sat_1KM_LU_ED_DU_SO4.tif",overwrite=TRUE)

R2_regression<- kriging_points(Output_r2, resl_ras= 0.01)
plot(R2_regression)
writeRaster(R2_regression, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/R2_regression.tif",overwrite=TRUE)

SE_regression<- kriging_points(Output_pred_se, resl_ras= 0.01)
plot(SE_regression)
writeRaster(SE_regression, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/to federico/SE_regression.tif",overwrite=TRUE)



#### significance test of the coefficients


## sig of intercept
sigTest_intercept = abs(dawit$X.Intercept.) -1.5 * dawit$X.Intercept._se
hist(sigTest_intercept,100)
sig_inter <-rasterFromXYZ(cbind(dawit[, c("x", "y")],sigTest_intercept), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(sig_inter>=0); cellStats ((sig_inter>=0 ), stat='sum', na.rm=TRUE)

## sig of Urban
sigTest_urban = abs(dawit$urban_fraction) -1.5 * dawit$urban_fraction_se
hist(sigTest_urban,100)
sig_urban <-rasterFromXYZ(cbind(dawit[, c("x", "y")],sigTest_urban), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(sig_urban>=0); cellStats ((sig_urban>=0 ), stat='sum', na.rm=TRUE)

## sig of desert
sigTest_desert = abs(dawit$desert_fraction) -1.5 * dawit$desert_fraction_se
hist(sigTest_desert,100)
sig_desert <-rasterFromXYZ(cbind(dawit[, c("x", "y")],sigTest_desert), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(sig_desert>=0); cellStats ((sig_desert>=0 ), stat='sum', na.rm=TRUE)

## sig of ED
sigTest_ED = abs(dawit$ED) -1.5 * dawit$ED_se
hist(sigTest_ED,100)
sig_ED <-rasterFromXYZ(cbind(dawit[, c("x", "y")],sigTest_ED), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(sig_ED>=0); cellStats ((sig_ED>=0 ), stat='sum', na.rm=TRUE)

## sig of DUST
sigTest_dust = abs(dawit$ECMWF_DUST_ECMWF_jan_10km) -1.5 * dawit$ECMWF_DUST_ECMWF_jan_10km_se
hist(sigTest_dust,100)
sig_dust <-rasterFromXYZ(cbind(dawit[, c("x", "y")],sigTest_dust), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(sig_dust>=0); cellStats ((sig_dust>=0 ), stat='sum', na.rm=TRUE)

## sig of SO4
sigTest_SO4 = abs(dawit$ECMWF_DUST_ECMWF_jan_10km) -1.5 * dawit$ECMWF_DUST_ECMWF_jan_10km_se
hist(sigTest_SO4,100)
sig_so4 <-rasterFromXYZ(cbind(dawit[, c("x", "y")],sigTest_SO4), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(sig_so4>=0); cellStats ((sig_so4>=0 ), stat='sum', na.rm=TRUE)





