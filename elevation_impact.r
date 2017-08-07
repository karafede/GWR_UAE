

DEM_uae<- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/GWR/DEM_UAE/DEM_UAE_cropped_30m.tif")
r_moni<- raster("in_situ_kriging_UAE.tif")
extent_uae<- extent(r_moni)
extent(DEM_uae)<-extent_uae

plot(DEM_uae)


DEM_uae_fact<-aggregate(DEM_uae, fact=363, fun=mean)

res(DEM_uae_fact)
res(DEM_uae)



DEM_uae_resa <- DEM_uae_fact
DEM_uae_resa[35,] <- 9999
DEM_uae_resa[,50] <- 9999
DEM_uae_resa<-trim(DEM_uae_resa,values=9999)
extent(DEM_uae_resa)<- extent(r_moni)

DEM_uae_resa_40km <-aggregate(DEM_uae_resa, fact=4, fun=mean)
DEM_uae_resa_40km_10km<-disaggregate(DEM_uae_resa_40km, fact=4, method="")

plot(DEM_uae_resa_40km)
plot(DEM_uae_resa_40km_10km)

DEM_uae_resa_40km_10km[35:36,] <- 9999
DEM_uae_resa_40km_10km[,50:52] <- 9999
DEM_uae_resa_40km_10km<-trim(DEM_uae_resa_40km_10km,values=9999)


ED<- DEM_uae_resa-DEM_uae_resa_40km_10km
ED <- mask(ED, r_moni)
ED_sp<- as(ED, 'SpatialPointsDataFrame')
names(ED_sp)<- "ED"
plot(ED)


writeRaster(ED, "D:/Air Quality/GWR/Saves from GWR script/ED_10km.tif", overwrite=TRUE)
res(ED)


plot(ED)
plot(DEM_uae_resa_40km_10km)

