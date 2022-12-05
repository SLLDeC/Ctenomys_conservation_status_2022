library(maptools)
library(raster)
library(rgeos)
library(ncdf4)
library(rasterVis)
library(mapview)
library(RColorBrewer)
library(gridExtra)
library(sf)
library(fasterize)

### coordinate systems
albers<-CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
Mollweide<-CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
wgs84<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

#Import Ctenomys ranges
AE<-st_read("data/ctenomys_distribution_areas_water_subtracted.shp")
AE$presence<-1
AEt<-st_transform(AE,Mollweide)


## Import land cover data obtained form Copernicus Climate Change Service for the years 2000 and 2020 at a scale of 300 m pixel size (https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-landcov-er?tab=overview).

LC2020<- raster("data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc", varname="lccs_class")

LC2000<- raster("data/Sep2021/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.nc", varname="lccs_class")

lc<-stack(LC2000,LC2020)

lc2<-crop(lc,AE) ## crop to study area

plot(lc2)

######## Reclassify land cover types: fied urban, rainfed, and irrigated/post-flooding agriculture, and permanent ice or snow land cover categories as anthropogenic
#intensive or unsuitable habitat land covers. The remaining land cover types as natural/seminatural.

m <- c(-1,1,NA, 1, 21, 0,  21, 181, 1, 181,191,0,191,203,1,203,221,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
nat2000 <- reclassify(lc2[[1]], rclmat)
my_rst_2000 <- ratify(nat2000)
levels(my_rst_2000)[[1]]$LC <- c("Anthropogenic","Natural/seminatural")
p1<-levelplot(my_rst_2000, colorkey=T, col.regions = c("red","green"), margin = FALSE,xlab.top="2000")


m <- c(-1,1,NA, 1, 21, 0,  21, 181, 100, 181,191,0,191,203,100,203,221,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
nat2020 <- reclassify(lc2[[2]], rclmat)
my_rst_2020 <- ratify(nat2020)
levels(my_rst_2020)[[1]]$LC <- c("Anthropogenic","Natural/seminatural")
p2<-levelplot(my_rst_2020, col.regions = c("red","green"), margin = FALSE,xlab.top="2018")


grid.arrange(p1, p2, ncol=2)


##### Change

chan<-nat2000+nat2020
table(values(chan))



my_chang <- ratify(chan)
levels(my_chang)[[1]]$LCC <- c("Stable_Anthropogenic","Loss natural","Gain natural","Stable_Natural")
levelplot(my_chang, col.regions = c("gray","red","green","darkgreen"), margin = FALSE,xlab.top="2018")


chan1<-projectRaster(chan, crs=Mollweide,method="ngb")

#### Quatify Change within the range of each Ctenomys species

sp<-unique(AEt$BINOMIA)


results<-data.frame(species=NA, area_rango=NA, area_natural_2010=NA,area_antropica_2010=NA,
                    area_natural_2020=NA,area_antropica_2020=NA, cambio_natural_neto=NA, porcentaje_natural_2020=NA,tasa_cambio=NA)

setwd("C:/Users/seboc/Box/Ctenomys/Sep2021/Raster_LandChange_May2022")
for (i in 1:length(sp)){
  
  results11<-data.frame(species=NA, area_rango=NA, area_natural_2010=NA,area_antropica_2010=NA,
                      area_natural_2020=NA,area_antropica_2020=NA, cambio_natural_neto=NA, porcentaje_natural_2020=NA,tasa_cambio=NA)
  
  
  r <- crop(chan1,AEt[i,])
  dist<- rasterize(AEt[i,], r, field = "presence")

  
  dist2<-resample(dist, r, method="ngb")
  
  dist2[values(dist2)==0]<-NA
  
  chandis<-dist2*r
  #ar<-raster::area(chandis)
  
 
  
  results11[1,"species"]<-AEt$BINOMIA[i]
  results11[1,"area_rango"]<-sum(values(dist2), na.rm=T)*0.0805
  
  A_nat_2010<-chandis
  A_nat_2010[values(A_nat_2010) %in% c(101,1)]<-1
  A_nat_2010[!values(A_nat_2010) %in% c(101,1)]<-0
  
  results11[1,"area_natural_2010"]<-sum(values(A_nat_2010), na.rm=T)*0.0805
  
  A_antr_2010<-chandis
  A_antr_2010[values(A_antr_2010) %in% c(0,100)]<-300
  A_antr_2010[values(A_antr_2010) %in% c(1,101)]<-0
  A_antr_2010[values(A_antr_2010) ==300]<-1
 
  results11[1,"area_antropica_2010"]<-sum(values(A_antr_2010), na.rm=T)*0.0805
  
  
  A_nat_2020<-chandis
  A_nat_2020[values(A_nat_2020) %in% c(101,100)]<-300
  A_nat_2020[values(A_nat_2020) %in% c(0,1)]<-0
  A_nat_2020[values(A_nat_2020) ==300]<-1
  
  results11[1,"area_natural_2020"]<- sum(values(A_nat_2020), na.rm=T)*0.0805
  
  A_antr_2020<-chandis
  A_antr_2020[values(A_antr_2020) %in% c(0,1)]<-300
  A_antr_2020[values(A_antr_2020) %in% c(101,100)]<-0
  A_antr_2020[values(A_antr_2020) ==300]<-1
  
  results11[1,"area_antropica_2020"]<-sum(values(A_antr_2020), na.rm=T)*0.0805
  
  results11[1,"cambio_natural_neto"]<-results11[1,"area_natural_2020"]-results11[1,"area_natural_2010"]
  results11[1,"porcentaje_natural_2020"]<- results11[1,"area_natural_2020"]/results11[1,"area_rango"]
  results11[1,"tasa_cambio"]<- (results11[1,"area_natural_2020"]-results11[1,"area_natural_2010"])/results11[1,"area_natural_2010"]
  
  
  results<-rbind(results,results11)
  
  print(i)
}


results$porcentaje_natural_2020<-results$porcentaje_natural_2020*100
results$tasa_cambio<-results$tasa_cambio*100

write.csv(results,"Cambio_coberturas.csv")
