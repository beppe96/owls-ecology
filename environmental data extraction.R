rm(list=ls())

setwd("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 2")

library(sf)
library(mapview)
library(ggmap)
library(terra)
library(dplyr)
library(stars)
library(mapview)
library(tidyterra)
library(reshape)


                                    ####################################
                                    ####################################
                                    #    LAND COVER DATA EXTRACTION    #
                                    ####################################
                                    ####################################


################
### SCOTLAND ###
################


LC_Scotland_2017_1 <- rast("2017_Land_Cover_Scotland_1.tif")
LC_Scotland_2017_2 <- rast("2017_Land_Cover_Scotland_2.tif")
LC_Scotland_2018_1 <- rast("2018_Land_Cover_Scotland_1.tif")
LC_Scotland_2018_2 <- rast("2018_Land_Cover_Scotland_2.tif")
LC_Scotland_2019_1 <- rast("2019_Land_Cover_Scotland_1.tif")
LC_Scotland_2019_2 <- rast("2019_Land_Cover_Scotland_2.tif")
LC_Scotland_2020_1 <- rast("2020_Land_Cover_Scotland_1.tif")
LC_Scotland_2020_2 <- rast("2020_Land_Cover_Scotland_2.tif")
LC_Scotland_2021_1 <- rast("2021_Land_Cover_Scotland_1.tif")
LC_Scotland_2021_2 <- rast("2021_Land_Cover_Scotland_2.tif")
LC_Scotland_2022_1 <- rast("2022_Land_Cover_Scotland_1.tif")
LC_Scotland_2022_2 <- rast("2022_Land_Cover_Scotland_2.tif")
LC_Scotland_2023_1 <- rast("2023_Land_Cover_Scotland_1.tif")
LC_Scotland_2023_2 <- rast("2023_Land_Cover_Scotland_2.tif")


LC_Scotland_2017 <- merge(LC_Scotland_2017_1, LC_Scotland_2017_2)
writeRaster(LC_Scotland_2017, filename = "2017_Land_Cover_Scotland.tif")

LC_Scotland_2018 <- merge(LC_Scotland_2018_1, LC_Scotland_2018_2)
writeRaster(LC_Scotland_2018, filename = "2018_Land_Cover_Scotland.tif")

LC_Scotland_2019 <- merge(LC_Scotland_2019_1, LC_Scotland_2019_2)
writeRaster(LC_Scotland_2019, filename = "2019_Land_Cover_Scotland.tif")

LC_Scotland_2020 <- merge(LC_Scotland_2020_1, LC_Scotland_2020_2)
writeRaster(LC_Scotland_2020, filename = "2020_Land_Cover_Scotland.tif")

LC_Scotland_2021 <- merge(LC_Scotland_2021_1, LC_Scotland_2021_2)
writeRaster(LC_Scotland_2021, filename = "2021_Land_Cover_Scotland.tif")

LC_Scotland_2022 <- merge(LC_Scotland_2022_1, LC_Scotland_2022_2)
writeRaster(LC_Scotland_2022, filename = "2022_Land_Cover_Scotland.tif")

LC_Scotland_2023 <- merge(LC_Scotland_2023_1, LC_Scotland_2023_2)
writeRaster(LC_Scotland_2023, filename = "2023_Land_Cover_Scotland.tif")


LC_Scotland_2017 <- rast("2017_Land_Cover_Scotland.tif")
LC_Scotland_2018 <- rast("2018_Land_Cover_Scotland.tif")
LC_Scotland_2019 <- rast("2019_Land_Cover_Scotland.tif")
LC_Scotland_2020 <- rast("2020_Land_Cover_Scotland.tif")
LC_Scotland_2021 <- rast("2021_Land_Cover_Scotland.tif")
LC_Scotland_2022 <- rast("2022_Land_Cover_Scotland.tif")
LC_Scotland_2023 <- rast("2023_Land_Cover_Scotland.tif")


owls_Scotland <- read.csv("Scotland data.csv")

owls_sf_Scotland <- st_as_sf(owls_Scotland, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Scotland)

mapview(owls_sf_Scotland)

radius_Scotland<-500 

owls_sf_Scotland$recno<-seq(1, nrow(owls_sf_Scotland),1)

buffer_500_Scotland<-st_buffer(st_transform(owls_sf_Scotland, "epsg:32630"),
                             dist = radius_Scotland) 

mapview(buffer_500_Scotland) 

# unique(values(LC_Scotland_2021))


variables_500_Scotland_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Scotland_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Scotland_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Scotland_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Scotland_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Scotland_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Scotland_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Scotland), ncol = 9)) 
names(variables_500_Scotland_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Scotland<-st_transform(buffer_500_Scotland, crs("epsg:32630"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Scotland)){
  int.list_2017[[i]] <- crop(LC_Scotland_2017,  buffer_500_Scotland[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Scotland_2018,  buffer_500_Scotland[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Scotland_2019,  buffer_500_Scotland[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Scotland_2020,  buffer_500_Scotland[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Scotland_2021,  buffer_500_Scotland[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Scotland_2022,  buffer_500_Scotland[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Scotland_2023,  buffer_500_Scotland[i,], mask=T)
  progress(i,  nrow(buffer_500_Scotland))
}

for (i in 1:nrow(buffer_500_Scotland)){
  for ( j in 1:length(names(variables_500_Scotland_2017))) {
    variables_500_Scotland_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Scotland_2017)[j])]
    variables_500_Scotland_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Scotland_2018)[j])]
    variables_500_Scotland_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Scotland_2019)[j])]
    variables_500_Scotland_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Scotland_2020)[j])]
    variables_500_Scotland_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Scotland_2021)[j])]
    variables_500_Scotland_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Scotland_2022)[j])]
    variables_500_Scotland_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Scotland_2023)[j])]
    
    progress(i,  nrow(buffer_500_Scotland))
  }
}


names(variables_500_Scotland_2017)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2018)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2019)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2020)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2021)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2022)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2023)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_Scotland_2017)
names(variables_500_Scotland_2018)
names(variables_500_Scotland_2019)
names(variables_500_Scotland_2020)
names(variables_500_Scotland_2021)
names(variables_500_Scotland_2022)
names(variables_500_Scotland_2023)

summary(variables_500_Scotland_2017)
summary(variables_500_Scotland_2018)
summary(variables_500_Scotland_2019)
summary(variables_500_Scotland_2020)
summary(variables_500_Scotland_2021)
summary(variables_500_Scotland_2022)
summary(variables_500_Scotland_2023)


variables_500_Scotland_2017[is.na(variables_500_Scotland_2017)]<-0
variables_500_Scotland_2018[is.na(variables_500_Scotland_2018)]<-0
variables_500_Scotland_2019[is.na(variables_500_Scotland_2019)]<-0
variables_500_Scotland_2020[is.na(variables_500_Scotland_2020)]<-0
variables_500_Scotland_2021[is.na(variables_500_Scotland_2021)]<-0
variables_500_Scotland_2022[is.na(variables_500_Scotland_2022)]<-0
variables_500_Scotland_2023[is.na(variables_500_Scotland_2023)]<-0


variables_500_Scotland_2017$Snow<-NULL
variables_500_Scotland_2017$Clouds<-NULL

variables_500_Scotland_2018$Snow<-NULL
variables_500_Scotland_2018$Clouds<-NULL

variables_500_Scotland_2019$Snow<-NULL
variables_500_Scotland_2019$Clouds<-NULL

variables_500_Scotland_2020$Snow<-NULL
variables_500_Scotland_2020$Clouds<-NULL

variables_500_Scotland_2021$Snow<-NULL
variables_500_Scotland_2021$Clouds<-NULL

variables_500_Scotland_2022$Snow<-NULL
variables_500_Scotland_2022$Clouds<-NULL

variables_500_Scotland_2023$Snow<-NULL
variables_500_Scotland_2023$Clouds<-NULL


variables_500_Scotland_2017$tot_pixel<-rowSums(variables_500_Scotland_2017)
variables_500_Scotland_2018$tot_pixel<-rowSums(variables_500_Scotland_2018)
variables_500_Scotland_2019$tot_pixel<-rowSums(variables_500_Scotland_2019)
variables_500_Scotland_2020$tot_pixel<-rowSums(variables_500_Scotland_2020)
variables_500_Scotland_2021$tot_pixel<-rowSums(variables_500_Scotland_2021)
variables_500_Scotland_2022$tot_pixel<-rowSums(variables_500_Scotland_2022)
variables_500_Scotland_2023$tot_pixel<-rowSums(variables_500_Scotland_2023)


#owls_sf_Scotland$Water_500<-NA
owls_sf_Scotland$Water_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Water/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Water_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Water/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Water_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Water/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Water_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Water/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Water_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Water/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Water_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Water/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Water_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Water/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]

owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Rangeland/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Rangeland/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Rangeland/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Rangeland/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Rangeland/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Rangeland/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Rangeland_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Rangeland/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]

owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Flood_veg/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Flood_veg/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Flood_veg/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Flood_veg/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Flood_veg/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Flood_veg/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Flood_veg_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Flood_veg/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]

owls_sf_Scotland$Crops_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Crops/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Crops_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Crops/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Crops_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Crops/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Crops_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Crops/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Crops_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Crops/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Crops_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Crops/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Crops_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Crops/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]

owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Bare_ground/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Bare_ground/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Bare_ground/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Bare_ground/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Bare_ground/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Bare_ground/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Bare_ground_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Bare_ground/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]

owls_sf_Scotland$Trees_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Trees/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Trees_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Trees/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Trees_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Trees/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Trees_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Trees/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Trees_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Trees/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Trees_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Trees/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Trees_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Trees/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]

owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year<=2017]<-round(variables_500_Scotland_2017$Built_up/variables_500_Scotland_2017$tot_pixel,3)[which(owls_sf_Scotland$year<=2017)]
owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year==2018]<-round(variables_500_Scotland_2018$Built_up/variables_500_Scotland_2018$tot_pixel,3)[which(owls_sf_Scotland$year==2018)]
owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year==2019]<-round(variables_500_Scotland_2019$Built_up/variables_500_Scotland_2019$tot_pixel,3)[which(owls_sf_Scotland$year==2019)]
owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year==2020]<-round(variables_500_Scotland_2020$Built_up/variables_500_Scotland_2020$tot_pixel,3)[which(owls_sf_Scotland$year==2020)]
owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year==2021]<-round(variables_500_Scotland_2021$Built_up/variables_500_Scotland_2021$tot_pixel,3)[which(owls_sf_Scotland$year==2021)]
owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year==2022]<-round(variables_500_Scotland_2022$Built_up/variables_500_Scotland_2022$tot_pixel,3)[which(owls_sf_Scotland$year==2022)]
owls_sf_Scotland$Built_up_500[owls_sf_Scotland$year==2023]<-round(variables_500_Scotland_2023$Built_up/variables_500_Scotland_2023$tot_pixel,3)[which(owls_sf_Scotland$year==2023)]




################
###  FRANCE  ###
################

LC_France_2017 <- rast("2017_Land_Cover_France.tif")
LC_France_2018 <- rast("2018_Land_Cover_France.tif")
LC_France_2019 <- rast("2019_Land_Cover_France.tif")
LC_France_2020 <- rast("2020_Land_Cover_France.tif")
LC_France_2021 <- rast("2021_Land_Cover_France.tif")
LC_France_2022 <- rast("2022_Land_Cover_France.tif")
LC_France_2023 <- rast("2023_Land_Cover_France.tif")


owls_France <- read.csv("France data.csv")

owls_sf_France <- st_as_sf(owls_France, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_France)

mapview(owls_sf_France)

radius_France<-500 

owls_sf_France$recno<-seq(1, nrow(owls_sf_France),1)

buffer_500_France<-st_buffer(st_transform(owls_sf_France, "epsg:32631"),
                             dist = radius_France) 

mapview(buffer_500_France) 

# unique(values(LC_France_2021))


variables_500_France_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_France_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_France_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_France_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_France_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_France_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_France_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_France), ncol = 9)) 
names(variables_500_France_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_France<-st_transform(buffer_500_France, crs("epsg:32631"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_France)){
  int.list_2017[[i]] <- crop(LC_France_2017,  buffer_500_France[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_France_2018,  buffer_500_France[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_France_2019,  buffer_500_France[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_France_2020,  buffer_500_France[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_France_2021,  buffer_500_France[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_France_2022,  buffer_500_France[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_France_2023,  buffer_500_France[i,], mask=T)
  progress(i,  nrow(buffer_500_France))
}

for (i in 1:nrow(buffer_500_France)){
  for ( j in 1:length(names(variables_500_France_2017))) {
    variables_500_France_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_France_2017)[j])]
    variables_500_France_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_France_2018)[j])]
    variables_500_France_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_France_2019)[j])]
    variables_500_France_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_France_2020)[j])]
    variables_500_France_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_France_2021)[j])]
    variables_500_France_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_France_2022)[j])]
    variables_500_France_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_France_2023)[j])]
    
    progress(i,  nrow(buffer_500_France))
  }
}


names(variables_500_France_2017)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2018)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2019)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2020)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2021)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2022)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2023)<-c("Water",
                                    "Rangeland",
                                    "Trees",
                                    "Built_up",
                                    "Snow",
                                    "Crops",
                                    "Bare_ground",
                                    "Flood_veg",
                                    "Clouds")

names(variables_500_France_2017)
names(variables_500_France_2018)
names(variables_500_France_2019)
names(variables_500_France_2020)
names(variables_500_France_2021)
names(variables_500_France_2022)
names(variables_500_France_2023)

summary(variables_500_France_2017)
summary(variables_500_France_2018)
summary(variables_500_France_2019)
summary(variables_500_France_2020)
summary(variables_500_France_2021)
summary(variables_500_France_2022)
summary(variables_500_France_2023)


variables_500_France_2017[is.na(variables_500_France_2017)]<-0
variables_500_France_2018[is.na(variables_500_France_2018)]<-0
variables_500_France_2019[is.na(variables_500_France_2019)]<-0
variables_500_France_2020[is.na(variables_500_France_2020)]<-0
variables_500_France_2021[is.na(variables_500_France_2021)]<-0
variables_500_France_2022[is.na(variables_500_France_2022)]<-0
variables_500_France_2023[is.na(variables_500_France_2023)]<-0


variables_500_France_2017$Snow<-NULL
variables_500_France_2017$Clouds<-NULL

variables_500_France_2018$Snow<-NULL
variables_500_France_2018$Clouds<-NULL

variables_500_France_2019$Snow<-NULL
variables_500_France_2019$Clouds<-NULL

variables_500_France_2020$Snow<-NULL
variables_500_France_2020$Clouds<-NULL

variables_500_France_2021$Snow<-NULL
variables_500_France_2021$Clouds<-NULL

variables_500_France_2022$Snow<-NULL
variables_500_France_2022$Clouds<-NULL

variables_500_France_2023$Snow<-NULL
variables_500_France_2023$Clouds<-NULL


variables_500_France_2017$tot_pixel<-rowSums(variables_500_France_2017)
variables_500_France_2018$tot_pixel<-rowSums(variables_500_France_2018)
variables_500_France_2019$tot_pixel<-rowSums(variables_500_France_2019)
variables_500_France_2020$tot_pixel<-rowSums(variables_500_France_2020)
variables_500_France_2021$tot_pixel<-rowSums(variables_500_France_2021)
variables_500_France_2022$tot_pixel<-rowSums(variables_500_France_2022)
variables_500_France_2023$tot_pixel<-rowSums(variables_500_France_2023)



#owls_sf_France$Water_500<-NA
owls_sf_France$Water_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Water/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Water_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Water/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Water_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Water/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Water_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Water/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Water_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Water/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Water_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Water/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Water_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Water/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]

owls_sf_France$Rangeland_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Rangeland/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Rangeland_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Rangeland/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Rangeland_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Rangeland/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Rangeland_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Rangeland/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Rangeland_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Rangeland/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Rangeland_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Rangeland/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Rangeland_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Rangeland/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]

owls_sf_France$Flood_veg_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Flood_veg/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Flood_veg_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Flood_veg/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Flood_veg_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Flood_veg/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Flood_veg_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Flood_veg/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Flood_veg_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Flood_veg/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Flood_veg_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Flood_veg/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Flood_veg_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Flood_veg/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]

owls_sf_France$Crops_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Crops/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Crops_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Crops/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Crops_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Crops/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Crops_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Crops/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Crops_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Crops/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Crops_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Crops/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Crops_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Crops/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]

owls_sf_France$Bare_ground_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Bare_ground/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Bare_ground_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Bare_ground/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Bare_ground_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Bare_ground/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Bare_ground_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Bare_ground/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Bare_ground_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Bare_ground/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Bare_ground_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Bare_ground/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Bare_ground_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Bare_ground/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]

owls_sf_France$Trees_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Trees/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Trees_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Trees/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Trees_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Trees/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Trees_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Trees/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Trees_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Trees/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Trees_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Trees/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Trees_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Trees/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]

owls_sf_France$Built_up_500[owls_sf_France$year<=2017]<-round(variables_500_France_2017$Built_up/variables_500_France_2017$tot_pixel,3)[which(owls_sf_France$year<=2017)]
owls_sf_France$Built_up_500[owls_sf_France$year==2018]<-round(variables_500_France_2018$Built_up/variables_500_France_2018$tot_pixel,3)[which(owls_sf_France$year==2018)]
owls_sf_France$Built_up_500[owls_sf_France$year==2019]<-round(variables_500_France_2019$Built_up/variables_500_France_2019$tot_pixel,3)[which(owls_sf_France$year==2019)]
owls_sf_France$Built_up_500[owls_sf_France$year==2020]<-round(variables_500_France_2020$Built_up/variables_500_France_2020$tot_pixel,3)[which(owls_sf_France$year==2020)]
owls_sf_France$Built_up_500[owls_sf_France$year==2021]<-round(variables_500_France_2021$Built_up/variables_500_France_2021$tot_pixel,3)[which(owls_sf_France$year==2021)]
owls_sf_France$Built_up_500[owls_sf_France$year==2022]<-round(variables_500_France_2022$Built_up/variables_500_France_2022$tot_pixel,3)[which(owls_sf_France$year==2022)]
owls_sf_France$Built_up_500[owls_sf_France$year==2023]<-round(variables_500_France_2023$Built_up/variables_500_France_2023$tot_pixel,3)[which(owls_sf_France$year==2023)]




################
###  SWEDEN  ###
################

LC_Sweden_2017 <- rast("2017_Land_Cover_Sweden.tif")
LC_Sweden_2018 <- rast("2018_Land_Cover_Sweden.tif")
LC_Sweden_2019 <- rast("2019_Land_Cover_Sweden.tif")
LC_Sweden_2020 <- rast("2020_Land_Cover_Sweden.tif")
LC_Sweden_2021 <- rast("2021_Land_Cover_Sweden.tif")
LC_Sweden_2022 <- rast("2022_Land_Cover_Sweden.tif")
LC_Sweden_2023 <- rast("2023_Land_Cover_Sweden.tif")


owls_Sweden <- read.csv("Sweden data.csv")

owls_sf_Sweden <- st_as_sf(owls_Sweden, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Sweden)

mapview(owls_sf_Sweden)

radius_Sweden<-500 

owls_sf_Sweden$recno<-seq(1, nrow(owls_sf_Sweden),1)

buffer_500_Sweden<-st_buffer(st_transform(owls_sf_Sweden, "epsg:32633"),
                               dist = radius_Sweden) 

mapview(buffer_500_Sweden) 

# unique(values(LC_Sweden_2021))


variables_500_Sweden_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Sweden_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Sweden_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Sweden_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Sweden_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Sweden_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Sweden_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Sweden), ncol = 9)) 
names(variables_500_Sweden_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Sweden<-st_transform(buffer_500_Sweden, crs("epsg:32633"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Sweden)){
  int.list_2017[[i]] <- crop(LC_Sweden_2017,  buffer_500_Sweden[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Sweden_2018,  buffer_500_Sweden[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Sweden_2019,  buffer_500_Sweden[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Sweden_2020,  buffer_500_Sweden[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Sweden_2021,  buffer_500_Sweden[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Sweden_2022,  buffer_500_Sweden[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Sweden_2023,  buffer_500_Sweden[i,], mask=T)
  progress(i,  nrow(buffer_500_Sweden))
}

for (i in 1:nrow(buffer_500_Sweden)){
  for ( j in 1:length(names(variables_500_Sweden_2017))) {
    variables_500_Sweden_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Sweden_2017)[j])]
    variables_500_Sweden_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Sweden_2018)[j])]
    variables_500_Sweden_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Sweden_2019)[j])]
    variables_500_Sweden_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Sweden_2020)[j])]
    variables_500_Sweden_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Sweden_2021)[j])]
    variables_500_Sweden_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Sweden_2022)[j])]
    variables_500_Sweden_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Sweden_2023)[j])]
    
    progress(i,  nrow(buffer_500_Sweden))
  }
}


names(variables_500_Sweden_2017)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2018)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2019)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2020)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2021)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2022)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2023)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Sweden_2017)
names(variables_500_Sweden_2018)
names(variables_500_Sweden_2019)
names(variables_500_Sweden_2020)
names(variables_500_Sweden_2021)
names(variables_500_Sweden_2022)
names(variables_500_Sweden_2023)

summary(variables_500_Sweden_2017)
summary(variables_500_Sweden_2018)
summary(variables_500_Sweden_2019)
summary(variables_500_Sweden_2020)
summary(variables_500_Sweden_2021)
summary(variables_500_Sweden_2022)
summary(variables_500_Sweden_2023)


variables_500_Sweden_2017[is.na(variables_500_Sweden_2017)]<-0
variables_500_Sweden_2018[is.na(variables_500_Sweden_2018)]<-0
variables_500_Sweden_2019[is.na(variables_500_Sweden_2019)]<-0
variables_500_Sweden_2020[is.na(variables_500_Sweden_2020)]<-0
variables_500_Sweden_2021[is.na(variables_500_Sweden_2021)]<-0
variables_500_Sweden_2022[is.na(variables_500_Sweden_2022)]<-0
variables_500_Sweden_2023[is.na(variables_500_Sweden_2023)]<-0


variables_500_Sweden_2017$Snow<-NULL
variables_500_Sweden_2017$Clouds<-NULL

variables_500_Sweden_2018$Snow<-NULL
variables_500_Sweden_2018$Clouds<-NULL

variables_500_Sweden_2019$Snow<-NULL
variables_500_Sweden_2019$Clouds<-NULL

variables_500_Sweden_2020$Snow<-NULL
variables_500_Sweden_2020$Clouds<-NULL

variables_500_Sweden_2021$Snow<-NULL
variables_500_Sweden_2021$Clouds<-NULL

variables_500_Sweden_2022$Snow<-NULL
variables_500_Sweden_2022$Clouds<-NULL

variables_500_Sweden_2023$Snow<-NULL
variables_500_Sweden_2023$Clouds<-NULL


variables_500_Sweden_2017$tot_pixel<-rowSums(variables_500_Sweden_2017)
variables_500_Sweden_2018$tot_pixel<-rowSums(variables_500_Sweden_2018)
variables_500_Sweden_2019$tot_pixel<-rowSums(variables_500_Sweden_2019)
variables_500_Sweden_2020$tot_pixel<-rowSums(variables_500_Sweden_2020)
variables_500_Sweden_2021$tot_pixel<-rowSums(variables_500_Sweden_2021)
variables_500_Sweden_2022$tot_pixel<-rowSums(variables_500_Sweden_2022)
variables_500_Sweden_2023$tot_pixel<-rowSums(variables_500_Sweden_2023)



#owls_sf_Sweden$Water_500<-NA
owls_sf_Sweden$Water_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Water/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Water_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Water/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Water_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Water/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Water_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Water/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Water_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Water/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Water_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Water/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Water_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Water/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]

owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Rangeland/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Rangeland/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Rangeland/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Rangeland/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Rangeland/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Rangeland/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Rangeland_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Rangeland/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]

owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Flood_veg/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Flood_veg/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Flood_veg/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Flood_veg/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Flood_veg/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Flood_veg/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Flood_veg_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Flood_veg/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]

owls_sf_Sweden$Crops_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Crops/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Crops_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Crops/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Crops_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Crops/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Crops_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Crops/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Crops_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Crops/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Crops_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Crops/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Crops_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Crops/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]

owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Bare_ground/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Bare_ground/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Bare_ground/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Bare_ground/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Bare_ground/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Bare_ground/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Bare_ground_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Bare_ground/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]

owls_sf_Sweden$Trees_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Trees/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Trees_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Trees/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Trees_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Trees/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Trees_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Trees/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Trees_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Trees/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Trees_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Trees/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Trees_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Trees/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]

owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year<=2017]<-round(variables_500_Sweden_2017$Built_up/variables_500_Sweden_2017$tot_pixel,3)[which(owls_sf_Sweden$year<=2017)]
owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year==2018]<-round(variables_500_Sweden_2018$Built_up/variables_500_Sweden_2018$tot_pixel,3)[which(owls_sf_Sweden$year==2018)]
owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year==2019]<-round(variables_500_Sweden_2019$Built_up/variables_500_Sweden_2019$tot_pixel,3)[which(owls_sf_Sweden$year==2019)]
owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year==2020]<-round(variables_500_Sweden_2020$Built_up/variables_500_Sweden_2020$tot_pixel,3)[which(owls_sf_Sweden$year==2020)]
owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year==2021]<-round(variables_500_Sweden_2021$Built_up/variables_500_Sweden_2021$tot_pixel,3)[which(owls_sf_Sweden$year==2021)]
owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year==2022]<-round(variables_500_Sweden_2022$Built_up/variables_500_Sweden_2022$tot_pixel,3)[which(owls_sf_Sweden$year==2022)]
owls_sf_Sweden$Built_up_500[owls_sf_Sweden$year==2023]<-round(variables_500_Sweden_2023$Built_up/variables_500_Sweden_2023$tot_pixel,3)[which(owls_sf_Sweden$year==2023)]




##################
###  SLOVENIA  ###
##################

LC_Slovenia_2017 <- rast("2017_Land_Cover_Slovenia.tif")
LC_Slovenia_2018 <- rast("2018_Land_Cover_Slovenia.tif")
LC_Slovenia_2019 <- rast("2019_Land_Cover_Slovenia.tif")
LC_Slovenia_2020 <- rast("2020_Land_Cover_Slovenia.tif")
LC_Slovenia_2021 <- rast("2021_Land_Cover_Slovenia.tif")
LC_Slovenia_2022 <- rast("2022_Land_Cover_Slovenia.tif")
LC_Slovenia_2023 <- rast("2023_Land_Cover_Slovenia.tif")


owls_Slovenia <- read.csv("Slovenia data.csv")

owls_sf_Slovenia <- st_as_sf(owls_Slovenia, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Slovenia)

mapview(owls_sf_Slovenia)

radius_Slovenia<-500 

owls_sf_Slovenia$recno<-seq(1, nrow(owls_sf_Slovenia),1)

buffer_500_Slovenia<-st_buffer(st_transform(owls_sf_Slovenia, "epsg:32633"),
                                  dist = radius_Slovenia) 

mapview(buffer_500_Slovenia) 

# unique(values(LC_Slovenia_2021))


variables_500_Slovenia_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Slovenia_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Slovenia_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Slovenia_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Slovenia_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Slovenia_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Slovenia_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Slovenia), ncol = 9)) 
names(variables_500_Slovenia_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Slovenia<-st_transform(buffer_500_Slovenia, crs("epsg:32633"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Slovenia)){
  int.list_2017[[i]] <- crop(LC_Slovenia_2017,  buffer_500_Slovenia[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Slovenia_2018,  buffer_500_Slovenia[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Slovenia_2019,  buffer_500_Slovenia[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Slovenia_2020,  buffer_500_Slovenia[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Slovenia_2021,  buffer_500_Slovenia[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Slovenia_2022,  buffer_500_Slovenia[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Slovenia_2023,  buffer_500_Slovenia[i,], mask=T)
  progress(i,  nrow(buffer_500_Slovenia))
}

for (i in 1:nrow(buffer_500_Slovenia)){
  for ( j in 1:length(names(variables_500_Slovenia_2017))) {
    variables_500_Slovenia_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Slovenia_2017)[j])]
    variables_500_Slovenia_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Slovenia_2018)[j])]
    variables_500_Slovenia_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Slovenia_2019)[j])]
    variables_500_Slovenia_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Slovenia_2020)[j])]
    variables_500_Slovenia_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Slovenia_2021)[j])]
    variables_500_Slovenia_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Slovenia_2022)[j])]
    variables_500_Slovenia_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Slovenia_2023)[j])]
    
    progress(i,  nrow(buffer_500_Slovenia))
  }
}


names(variables_500_Slovenia_2017)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2018)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2019)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2020)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2021)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2022)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2023)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Slovenia_2017)
names(variables_500_Slovenia_2018)
names(variables_500_Slovenia_2019)
names(variables_500_Slovenia_2020)
names(variables_500_Slovenia_2021)
names(variables_500_Slovenia_2022)
names(variables_500_Slovenia_2023)

summary(variables_500_Slovenia_2017)
summary(variables_500_Slovenia_2018)
summary(variables_500_Slovenia_2019)
summary(variables_500_Slovenia_2020)
summary(variables_500_Slovenia_2021)
summary(variables_500_Slovenia_2022)
summary(variables_500_Slovenia_2023)


variables_500_Slovenia_2017[is.na(variables_500_Slovenia_2017)]<-0
variables_500_Slovenia_2018[is.na(variables_500_Slovenia_2018)]<-0
variables_500_Slovenia_2019[is.na(variables_500_Slovenia_2019)]<-0
variables_500_Slovenia_2020[is.na(variables_500_Slovenia_2020)]<-0
variables_500_Slovenia_2021[is.na(variables_500_Slovenia_2021)]<-0
variables_500_Slovenia_2022[is.na(variables_500_Slovenia_2022)]<-0
variables_500_Slovenia_2023[is.na(variables_500_Slovenia_2023)]<-0


variables_500_Slovenia_2017$Snow<-NULL
variables_500_Slovenia_2017$Clouds<-NULL

variables_500_Slovenia_2018$Snow<-NULL
variables_500_Slovenia_2018$Clouds<-NULL

variables_500_Slovenia_2019$Snow<-NULL
variables_500_Slovenia_2019$Clouds<-NULL

variables_500_Slovenia_2020$Snow<-NULL
variables_500_Slovenia_2020$Clouds<-NULL

variables_500_Slovenia_2021$Snow<-NULL
variables_500_Slovenia_2021$Clouds<-NULL

variables_500_Slovenia_2022$Snow<-NULL
variables_500_Slovenia_2022$Clouds<-NULL

variables_500_Slovenia_2023$Snow<-NULL
variables_500_Slovenia_2023$Clouds<-NULL


variables_500_Slovenia_2017$tot_pixel<-rowSums(variables_500_Slovenia_2017)
variables_500_Slovenia_2018$tot_pixel<-rowSums(variables_500_Slovenia_2018)
variables_500_Slovenia_2019$tot_pixel<-rowSums(variables_500_Slovenia_2019)
variables_500_Slovenia_2020$tot_pixel<-rowSums(variables_500_Slovenia_2020)
variables_500_Slovenia_2021$tot_pixel<-rowSums(variables_500_Slovenia_2021)
variables_500_Slovenia_2022$tot_pixel<-rowSums(variables_500_Slovenia_2022)
variables_500_Slovenia_2023$tot_pixel<-rowSums(variables_500_Slovenia_2023)



#owls_sf_Slovenia$Water_500<-NA
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Water/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Water/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Water/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Water/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Water/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Water/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Water_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Water/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]

owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Rangeland/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Rangeland/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Rangeland/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Rangeland/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Rangeland/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Rangeland/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Rangeland_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Rangeland/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]

owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Flood_veg/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Flood_veg/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Flood_veg/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Flood_veg/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Flood_veg/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Flood_veg/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Flood_veg_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Flood_veg/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]

owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Crops/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Crops/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Crops/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Crops/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Crops/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Crops/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Crops_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Crops/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]

owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Bare_ground/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Bare_ground/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Bare_ground/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Bare_ground/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Bare_ground/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Bare_ground/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Bare_ground_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Bare_ground/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]

owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Trees/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Trees/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Trees/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Trees/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Trees/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Trees/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Trees_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Trees/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]

owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year<=2017]<-round(variables_500_Slovenia_2017$Built_up/variables_500_Slovenia_2017$tot_pixel,3)[which(owls_sf_Slovenia$year<=2017)]
owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year==2018]<-round(variables_500_Slovenia_2018$Built_up/variables_500_Slovenia_2018$tot_pixel,3)[which(owls_sf_Slovenia$year==2018)]
owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year==2019]<-round(variables_500_Slovenia_2019$Built_up/variables_500_Slovenia_2019$tot_pixel,3)[which(owls_sf_Slovenia$year==2019)]
owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year==2020]<-round(variables_500_Slovenia_2020$Built_up/variables_500_Slovenia_2020$tot_pixel,3)[which(owls_sf_Slovenia$year==2020)]
owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year==2021]<-round(variables_500_Slovenia_2021$Built_up/variables_500_Slovenia_2021$tot_pixel,3)[which(owls_sf_Slovenia$year==2021)]
owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year==2022]<-round(variables_500_Slovenia_2022$Built_up/variables_500_Slovenia_2022$tot_pixel,3)[which(owls_sf_Slovenia$year==2022)]
owls_sf_Slovenia$Built_up_500[owls_sf_Slovenia$year==2023]<-round(variables_500_Slovenia_2023$Built_up/variables_500_Slovenia_2023$tot_pixel,3)[which(owls_sf_Slovenia$year==2023)]



#####################
###  SWITZERLAND  ###
#####################


LC_Switzerland_2017 <- rast("2017_Land_Cover_Switzerland.tif")
LC_Switzerland_2018 <- rast("2018_Land_Cover_Switzerland.tif")
LC_Switzerland_2019 <- rast("2019_Land_Cover_Switzerland.tif")
LC_Switzerland_2020 <- rast("2020_Land_Cover_Switzerland.tif")
LC_Switzerland_2021 <- rast("2021_Land_Cover_Switzerland.tif")
LC_Switzerland_2022 <- rast("2022_Land_Cover_Switzerland.tif")
LC_Switzerland_2023 <- rast("2023_Land_Cover_Switzerland.tif")


owls_Switzerland <- read.csv("Switzerland data.csv")

owls_sf_Switzerland <- st_as_sf(owls_Switzerland, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Switzerland)

mapview(owls_sf_Switzerland)

radius_Switzerland<-500 

owls_sf_Switzerland$recno<-seq(1, nrow(owls_sf_Switzerland),1)

buffer_500_Switzerland<-st_buffer(st_transform(owls_sf_Switzerland, "epsg:32632"),
                               dist = radius_Switzerland) 

mapview(buffer_500_Switzerland) 

# unique(values(LC_Switzerland_2021))


variables_500_Switzerland_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Switzerland_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Switzerland_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Switzerland_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Switzerland_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Switzerland_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Switzerland_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Switzerland), ncol = 9)) 
names(variables_500_Switzerland_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Switzerland<-st_transform(buffer_500_Switzerland, crs("epsg:32632"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Switzerland)){
  int.list_2017[[i]] <- crop(LC_Switzerland_2017,  buffer_500_Switzerland[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Switzerland_2018,  buffer_500_Switzerland[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Switzerland_2019,  buffer_500_Switzerland[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Switzerland_2020,  buffer_500_Switzerland[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Switzerland_2021,  buffer_500_Switzerland[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Switzerland_2022,  buffer_500_Switzerland[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Switzerland_2023,  buffer_500_Switzerland[i,], mask=T)
  progress(i,  nrow(buffer_500_Switzerland))
}

for (i in 1:nrow(buffer_500_Switzerland)){
  for ( j in 1:length(names(variables_500_Switzerland_2017))) {
    variables_500_Switzerland_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Switzerland_2017)[j])]
    variables_500_Switzerland_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Switzerland_2018)[j])]
    variables_500_Switzerland_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Switzerland_2019)[j])]
    variables_500_Switzerland_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Switzerland_2020)[j])]
    variables_500_Switzerland_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Switzerland_2021)[j])]
    variables_500_Switzerland_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Switzerland_2022)[j])]
    variables_500_Switzerland_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Switzerland_2023)[j])]
    
    progress(i,  nrow(buffer_500_Switzerland))
  }
}


names(variables_500_Switzerland_2017)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Switzerland_2018)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Switzerland_2019)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Switzerland_2020)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Switzerland_2021)<-c("Water",
                                 "Rangeland",
                                 "Trees",
                                 "Built_up",
                                 "Snow",
                                 "Crops",
                                 "Bare_ground",
                                 "Flood_veg",
                                 "Clouds")

names(variables_500_Switzerland_2022)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Switzerland_2023)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Switzerland_2017)
names(variables_500_Switzerland_2018)
names(variables_500_Switzerland_2019)
names(variables_500_Switzerland_2020)
names(variables_500_Switzerland_2021)
names(variables_500_Switzerland_2022)
names(variables_500_Switzerland_2023)

summary(variables_500_Switzerland_2017)
summary(variables_500_Switzerland_2018)
summary(variables_500_Switzerland_2019)
summary(variables_500_Switzerland_2020)
summary(variables_500_Switzerland_2021)
summary(variables_500_Switzerland_2022)
summary(variables_500_Switzerland_2023)


variables_500_Switzerland_2017[is.na(variables_500_Switzerland_2017)]<-0
variables_500_Switzerland_2018[is.na(variables_500_Switzerland_2018)]<-0
variables_500_Switzerland_2019[is.na(variables_500_Switzerland_2019)]<-0
variables_500_Switzerland_2020[is.na(variables_500_Switzerland_2020)]<-0
variables_500_Switzerland_2021[is.na(variables_500_Switzerland_2021)]<-0
variables_500_Switzerland_2022[is.na(variables_500_Switzerland_2022)]<-0
variables_500_Switzerland_2023[is.na(variables_500_Switzerland_2023)]<-0


variables_500_Switzerland_2017$Snow<-NULL
variables_500_Switzerland_2017$Clouds<-NULL

variables_500_Switzerland_2018$Snow<-NULL
variables_500_Switzerland_2018$Clouds<-NULL

variables_500_Switzerland_2019$Snow<-NULL
variables_500_Switzerland_2019$Clouds<-NULL

variables_500_Switzerland_2020$Snow<-NULL
variables_500_Switzerland_2020$Clouds<-NULL

variables_500_Switzerland_2021$Snow<-NULL
variables_500_Switzerland_2021$Clouds<-NULL

variables_500_Switzerland_2022$Snow<-NULL
variables_500_Switzerland_2022$Clouds<-NULL

variables_500_Switzerland_2023$Snow<-NULL
variables_500_Switzerland_2023$Clouds<-NULL


variables_500_Switzerland_2017$tot_pixel<-rowSums(variables_500_Switzerland_2017)
variables_500_Switzerland_2018$tot_pixel<-rowSums(variables_500_Switzerland_2018)
variables_500_Switzerland_2019$tot_pixel<-rowSums(variables_500_Switzerland_2019)
variables_500_Switzerland_2020$tot_pixel<-rowSums(variables_500_Switzerland_2020)
variables_500_Switzerland_2021$tot_pixel<-rowSums(variables_500_Switzerland_2021)
variables_500_Switzerland_2022$tot_pixel<-rowSums(variables_500_Switzerland_2022)
variables_500_Switzerland_2023$tot_pixel<-rowSums(variables_500_Switzerland_2023)



#owls_sf_Switzerland$Water_500<-NA
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Water/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Water/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Water/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Water/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Water/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Water/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Water_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Water/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]

owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Rangeland/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Rangeland/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Rangeland/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Rangeland/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Rangeland/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Rangeland/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Rangeland_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Rangeland/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]

owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Flood_veg/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Flood_veg/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Flood_veg/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Flood_veg/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Flood_veg/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Flood_veg/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Flood_veg_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Flood_veg/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]

owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Crops/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Crops/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Crops/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Crops/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Crops/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Crops/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Crops_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Crops/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]

owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Bare_ground/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Bare_ground/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Bare_ground/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Bare_ground/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Bare_ground/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Bare_ground/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Bare_ground_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Bare_ground/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]

owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Trees/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Trees/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Trees/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Trees/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Trees/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Trees/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Trees_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Trees/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]

owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year<=2017]<-round(variables_500_Switzerland_2017$Built_up/variables_500_Switzerland_2017$tot_pixel,3)[which(owls_sf_Switzerland$year<=2017)]
owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year==2018]<-round(variables_500_Switzerland_2018$Built_up/variables_500_Switzerland_2018$tot_pixel,3)[which(owls_sf_Switzerland$year==2018)]
owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year==2019]<-round(variables_500_Switzerland_2019$Built_up/variables_500_Switzerland_2019$tot_pixel,3)[which(owls_sf_Switzerland$year==2019)]
owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year==2020]<-round(variables_500_Switzerland_2020$Built_up/variables_500_Switzerland_2020$tot_pixel,3)[which(owls_sf_Switzerland$year==2020)]
owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year==2021]<-round(variables_500_Switzerland_2021$Built_up/variables_500_Switzerland_2021$tot_pixel,3)[which(owls_sf_Switzerland$year==2021)]
owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year==2022]<-round(variables_500_Switzerland_2022$Built_up/variables_500_Switzerland_2022$tot_pixel,3)[which(owls_sf_Switzerland$year==2022)]
owls_sf_Switzerland$Built_up_500[owls_sf_Switzerland$year==2023]<-round(variables_500_Switzerland_2023$Built_up/variables_500_Switzerland_2023$tot_pixel,3)[which(owls_sf_Switzerland$year==2023)]




################
###  CZECHIA ###
################

LC_Czechia_2017 <- rast("2017_Land_Cover_Czechia.tif")
LC_Czechia_2018 <- rast("2018_Land_Cover_Czechia.tif")
LC_Czechia_2019 <- rast("2019_Land_Cover_Czechia.tif")
LC_Czechia_2020 <- rast("2020_Land_Cover_Czechia.tif")
LC_Czechia_2021 <- rast("2021_Land_Cover_Czechia.tif")
LC_Czechia_2022 <- rast("2022_Land_Cover_Czechia.tif")
LC_Czechia_2023 <- rast("2023_Land_Cover_Czechia.tif")


owls_Czechia <- read.csv("Czechia data.csv")

owls_sf_Czechia <- st_as_sf(owls_Czechia, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Czechia)

mapview(owls_sf_Czechia)

radius_Czechia<-500 

owls_sf_Czechia$recno<-seq(1, nrow(owls_sf_Czechia),1)

buffer_500_Czechia<-st_buffer(st_transform(owls_sf_Czechia, "epsg:32633"),
                                  dist = radius_Czechia) 

mapview(buffer_500_Czechia) 

# unique(values(LC_Czechia_2021))


variables_500_Czechia_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Czechia_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Czechia_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Czechia_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Czechia_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Czechia_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Czechia_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Czechia), ncol = 9)) 
names(variables_500_Czechia_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Czechia<-st_transform(buffer_500_Czechia, crs("epsg:32633"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Czechia)){
  int.list_2017[[i]] <- crop(LC_Czechia_2017,  buffer_500_Czechia[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Czechia_2018,  buffer_500_Czechia[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Czechia_2019,  buffer_500_Czechia[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Czechia_2020,  buffer_500_Czechia[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Czechia_2021,  buffer_500_Czechia[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Czechia_2022,  buffer_500_Czechia[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Czechia_2023,  buffer_500_Czechia[i,], mask=T)
  progress(i,  nrow(buffer_500_Czechia))
}

for (i in 1:nrow(buffer_500_Czechia)){
  for ( j in 1:length(names(variables_500_Czechia_2017))) {
    variables_500_Czechia_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Czechia_2017)[j])]
    variables_500_Czechia_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Czechia_2018)[j])]
    variables_500_Czechia_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Czechia_2019)[j])]
    variables_500_Czechia_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Czechia_2020)[j])]
    variables_500_Czechia_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Czechia_2021)[j])]
    variables_500_Czechia_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Czechia_2022)[j])]
    variables_500_Czechia_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Czechia_2023)[j])]
    
    progress(i,  nrow(buffer_500_Czechia))
  }
}


names(variables_500_Czechia_2017)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2018)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2019)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2020)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2021)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2022)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2023)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Czechia_2017)
names(variables_500_Czechia_2018)
names(variables_500_Czechia_2019)
names(variables_500_Czechia_2020)
names(variables_500_Czechia_2021)
names(variables_500_Czechia_2022)
names(variables_500_Czechia_2023)

summary(variables_500_Czechia_2017)
summary(variables_500_Czechia_2018)
summary(variables_500_Czechia_2019)
summary(variables_500_Czechia_2020)
summary(variables_500_Czechia_2021)
summary(variables_500_Czechia_2022)
summary(variables_500_Czechia_2023)


variables_500_Czechia_2017[is.na(variables_500_Czechia_2017)]<-0
variables_500_Czechia_2018[is.na(variables_500_Czechia_2018)]<-0
variables_500_Czechia_2019[is.na(variables_500_Czechia_2019)]<-0
variables_500_Czechia_2020[is.na(variables_500_Czechia_2020)]<-0
variables_500_Czechia_2021[is.na(variables_500_Czechia_2021)]<-0
variables_500_Czechia_2022[is.na(variables_500_Czechia_2022)]<-0
variables_500_Czechia_2023[is.na(variables_500_Czechia_2023)]<-0


variables_500_Czechia_2017$Snow<-NULL
variables_500_Czechia_2017$Clouds<-NULL

variables_500_Czechia_2018$Snow<-NULL
variables_500_Czechia_2018$Clouds<-NULL

variables_500_Czechia_2019$Snow<-NULL
variables_500_Czechia_2019$Clouds<-NULL

variables_500_Czechia_2020$Snow<-NULL
variables_500_Czechia_2020$Clouds<-NULL

variables_500_Czechia_2021$Snow<-NULL
variables_500_Czechia_2021$Clouds<-NULL

variables_500_Czechia_2022$Snow<-NULL
variables_500_Czechia_2022$Clouds<-NULL

variables_500_Czechia_2023$Snow<-NULL
variables_500_Czechia_2023$Clouds<-NULL


variables_500_Czechia_2017$tot_pixel<-rowSums(variables_500_Czechia_2017)
variables_500_Czechia_2018$tot_pixel<-rowSums(variables_500_Czechia_2018)
variables_500_Czechia_2019$tot_pixel<-rowSums(variables_500_Czechia_2019)
variables_500_Czechia_2020$tot_pixel<-rowSums(variables_500_Czechia_2020)
variables_500_Czechia_2021$tot_pixel<-rowSums(variables_500_Czechia_2021)
variables_500_Czechia_2022$tot_pixel<-rowSums(variables_500_Czechia_2022)
variables_500_Czechia_2023$tot_pixel<-rowSums(variables_500_Czechia_2023)



#owls_sf_Czechia$Water_500<-NA
owls_sf_Czechia$Water_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Water/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Water_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Water/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Water_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Water/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Water_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Water/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Water_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Water/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Water_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Water/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Water_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Water/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]

owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Rangeland/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Rangeland/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Rangeland/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Rangeland/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Rangeland/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Rangeland/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Rangeland_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Rangeland/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]

owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Flood_veg/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Flood_veg/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Flood_veg/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Flood_veg/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Flood_veg/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Flood_veg/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Flood_veg_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Flood_veg/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]

owls_sf_Czechia$Crops_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Crops/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Crops_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Crops/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Crops_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Crops/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Crops_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Crops/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Crops_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Crops/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Crops_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Crops/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Crops_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Crops/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]

owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Bare_ground/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Bare_ground/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Bare_ground/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Bare_ground/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Bare_ground/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Bare_ground/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Bare_ground_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Bare_ground/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]

owls_sf_Czechia$Trees_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Trees/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Trees_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Trees/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Trees_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Trees/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Trees_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Trees/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Trees_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Trees/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Trees_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Trees/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Trees_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Trees/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]

owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year<=2017]<-round(variables_500_Czechia_2017$Built_up/variables_500_Czechia_2017$tot_pixel,3)[which(owls_sf_Czechia$year<=2017)]
owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year==2018]<-round(variables_500_Czechia_2018$Built_up/variables_500_Czechia_2018$tot_pixel,3)[which(owls_sf_Czechia$year==2018)]
owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year==2019]<-round(variables_500_Czechia_2019$Built_up/variables_500_Czechia_2019$tot_pixel,3)[which(owls_sf_Czechia$year==2019)]
owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year==2020]<-round(variables_500_Czechia_2020$Built_up/variables_500_Czechia_2020$tot_pixel,3)[which(owls_sf_Czechia$year==2020)]
owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year==2021]<-round(variables_500_Czechia_2021$Built_up/variables_500_Czechia_2021$tot_pixel,3)[which(owls_sf_Czechia$year==2021)]
owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year==2022]<-round(variables_500_Czechia_2022$Built_up/variables_500_Czechia_2022$tot_pixel,3)[which(owls_sf_Czechia$year==2022)]
owls_sf_Czechia$Built_up_500[owls_sf_Czechia$year==2023]<-round(variables_500_Czechia_2023$Built_up/variables_500_Czechia_2023$tot_pixel,3)[which(owls_sf_Czechia$year==2023)]



####################
###  LITHUANIA a ###
####################

LC_Lithuania_a_2017 <- rast("2017_Land_Cover_Lithuania_a.tif")
LC_Lithuania_a_2018 <- rast("2018_Land_Cover_Lithuania_a.tif")
LC_Lithuania_a_2019 <- rast("2019_Land_Cover_Lithuania_a.tif")
LC_Lithuania_a_2020 <- rast("2020_Land_Cover_Lithuania_a.tif")
LC_Lithuania_a_2021 <- rast("2021_Land_Cover_Lithuania_a.tif")
LC_Lithuania_a_2022 <- rast("2022_Land_Cover_Lithuania_a.tif")
LC_Lithuania_a_2023 <- rast("2023_Land_Cover_Lithuania_a.tif")


owls_Lithuania_a <- read.csv("Lithuania data a.csv")

owls_sf_Lithuania_a <- st_as_sf(owls_Lithuania_a, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Lithuania_a)

mapview(owls_sf_Lithuania_a)

radius_Lithuania_a<-500 

owls_sf_Lithuania_a$recno<-seq(1, nrow(owls_sf_Lithuania_a),1)

buffer_500_Lithuania_a<-st_buffer(st_transform(owls_sf_Lithuania_a, "epsg:32634"),
                              dist = radius_Lithuania_a) 

mapview(buffer_500_Lithuania_a) 

# unique(values(LC_Lithuania_a_2021))


variables_500_Lithuania_a_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_a_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_a_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_a_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_a_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_a_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_a_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_a), ncol = 9)) 
names(variables_500_Lithuania_a_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Lithuania_a<-st_transform(buffer_500_Lithuania_a, crs("epsg:32634"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Lithuania_a)){
  int.list_2017[[i]] <- crop(LC_Lithuania_a_2017,  buffer_500_Lithuania_a[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Lithuania_a_2018,  buffer_500_Lithuania_a[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Lithuania_a_2019,  buffer_500_Lithuania_a[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Lithuania_a_2020,  buffer_500_Lithuania_a[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Lithuania_a_2021,  buffer_500_Lithuania_a[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Lithuania_a_2022,  buffer_500_Lithuania_a[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Lithuania_a_2023,  buffer_500_Lithuania_a[i,], mask=T)
  progress(i,  nrow(buffer_500_Lithuania_a))
}

for (i in 1:nrow(buffer_500_Lithuania_a)){
  for ( j in 1:length(names(variables_500_Lithuania_a_2017))) {
    variables_500_Lithuania_a_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Lithuania_a_2017)[j])]
    variables_500_Lithuania_a_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Lithuania_a_2018)[j])]
    variables_500_Lithuania_a_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Lithuania_a_2019)[j])]
    variables_500_Lithuania_a_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Lithuania_a_2020)[j])]
    variables_500_Lithuania_a_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Lithuania_a_2021)[j])]
    variables_500_Lithuania_a_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Lithuania_a_2022)[j])]
    variables_500_Lithuania_a_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Lithuania_a_2023)[j])]
    
    progress(i,  nrow(buffer_500_Lithuania_a))
  }
}


names(variables_500_Lithuania_a_2017)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")

names(variables_500_Lithuania_a_2018)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")

names(variables_500_Lithuania_a_2019)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")

names(variables_500_Lithuania_a_2020)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")

names(variables_500_Lithuania_a_2021)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")

names(variables_500_Lithuania_a_2022)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")

names(variables_500_Lithuania_a_2023)<-c("Water",
                                     "Rangeland",
                                     "Trees",
                                     "Built_up",
                                     "Snow",
                                     "Crops",
                                     "Bare_ground",
                                     "Flood_veg",
                                     "Clouds")
names(variables_500_Lithuania_a_2017)
names(variables_500_Lithuania_a_2018)
names(variables_500_Lithuania_a_2019)
names(variables_500_Lithuania_a_2020)
names(variables_500_Lithuania_a_2021)
names(variables_500_Lithuania_a_2022)
names(variables_500_Lithuania_a_2023)

summary(variables_500_Lithuania_a_2017)
summary(variables_500_Lithuania_a_2018)
summary(variables_500_Lithuania_a_2019)
summary(variables_500_Lithuania_a_2020)
summary(variables_500_Lithuania_a_2021)
summary(variables_500_Lithuania_a_2022)
summary(variables_500_Lithuania_a_2023)


variables_500_Lithuania_a_2017[is.na(variables_500_Lithuania_a_2017)]<-0
variables_500_Lithuania_a_2018[is.na(variables_500_Lithuania_a_2018)]<-0
variables_500_Lithuania_a_2019[is.na(variables_500_Lithuania_a_2019)]<-0
variables_500_Lithuania_a_2020[is.na(variables_500_Lithuania_a_2020)]<-0
variables_500_Lithuania_a_2021[is.na(variables_500_Lithuania_a_2021)]<-0
variables_500_Lithuania_a_2022[is.na(variables_500_Lithuania_a_2022)]<-0
variables_500_Lithuania_a_2023[is.na(variables_500_Lithuania_a_2023)]<-0


variables_500_Lithuania_a_2017$Snow<-NULL
variables_500_Lithuania_a_2017$Clouds<-NULL

variables_500_Lithuania_a_2018$Snow<-NULL
variables_500_Lithuania_a_2018$Clouds<-NULL

variables_500_Lithuania_a_2019$Snow<-NULL
variables_500_Lithuania_a_2019$Clouds<-NULL

variables_500_Lithuania_a_2020$Snow<-NULL
variables_500_Lithuania_a_2020$Clouds<-NULL

variables_500_Lithuania_a_2021$Snow<-NULL
variables_500_Lithuania_a_2021$Clouds<-NULL

variables_500_Lithuania_a_2022$Snow<-NULL
variables_500_Lithuania_a_2022$Clouds<-NULL

variables_500_Lithuania_a_2023$Snow<-NULL
variables_500_Lithuania_a_2023$Clouds<-NULL


variables_500_Lithuania_a_2017$tot_pixel<-rowSums(variables_500_Lithuania_a_2017)
variables_500_Lithuania_a_2018$tot_pixel<-rowSums(variables_500_Lithuania_a_2018)
variables_500_Lithuania_a_2019$tot_pixel<-rowSums(variables_500_Lithuania_a_2019)
variables_500_Lithuania_a_2020$tot_pixel<-rowSums(variables_500_Lithuania_a_2020)
variables_500_Lithuania_a_2021$tot_pixel<-rowSums(variables_500_Lithuania_a_2021)
variables_500_Lithuania_a_2022$tot_pixel<-rowSums(variables_500_Lithuania_a_2022)
variables_500_Lithuania_a_2023$tot_pixel<-rowSums(variables_500_Lithuania_a_2023)



#owls_sf_Lithuania_a$Water_500<-NA
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Water/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Water/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Water/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Water/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Water/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Water/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Water_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Water/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]

owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Rangeland/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Rangeland/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Rangeland/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Rangeland/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Rangeland/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Rangeland/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Rangeland_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Rangeland/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]

owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Flood_veg/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Flood_veg/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Flood_veg/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Flood_veg/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Flood_veg/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Flood_veg/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Flood_veg_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Flood_veg/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]

owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Crops/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Crops/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Crops/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Crops/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Crops/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Crops/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Crops_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Crops/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]

owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Bare_ground/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Bare_ground/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Bare_ground/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Bare_ground/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Bare_ground/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Bare_ground/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Bare_ground_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Bare_ground/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]

owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Trees/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Trees/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Trees/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Trees/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Trees/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Trees/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Trees_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Trees/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]

owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year<=2017]<-round(variables_500_Lithuania_a_2017$Built_up/variables_500_Lithuania_a_2017$tot_pixel,3)[which(owls_sf_Lithuania_a$year<=2017)]
owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year==2018]<-round(variables_500_Lithuania_a_2018$Built_up/variables_500_Lithuania_a_2018$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2018)]
owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year==2019]<-round(variables_500_Lithuania_a_2019$Built_up/variables_500_Lithuania_a_2019$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2019)]
owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year==2020]<-round(variables_500_Lithuania_a_2020$Built_up/variables_500_Lithuania_a_2020$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2020)]
owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year==2021]<-round(variables_500_Lithuania_a_2021$Built_up/variables_500_Lithuania_a_2021$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2021)]
owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year==2022]<-round(variables_500_Lithuania_a_2022$Built_up/variables_500_Lithuania_a_2022$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2022)]
owls_sf_Lithuania_a$Built_up_500[owls_sf_Lithuania_a$year==2023]<-round(variables_500_Lithuania_a_2023$Built_up/variables_500_Lithuania_a_2023$tot_pixel,3)[which(owls_sf_Lithuania_a$year==2023)]




####################
###  LITHUANIA b ###
####################

LC_Lithuania_b_2017 <- rast("2017_Land_Cover_Lithuania_b.tif")
LC_Lithuania_b_2018 <- rast("2018_Land_Cover_Lithuania_b.tif")
LC_Lithuania_b_2019 <- rast("2019_Land_Cover_Lithuania_b.tif")
LC_Lithuania_b_2020 <- rast("2020_Land_Cover_Lithuania_b.tif")
LC_Lithuania_b_2021 <- rast("2021_Land_Cover_Lithuania_b.tif")
LC_Lithuania_b_2022 <- rast("2022_Land_Cover_Lithuania_b.tif")
LC_Lithuania_b_2023 <- rast("2023_Land_Cover_Lithuania_b.tif")


owls_Lithuania_b <- read.csv("Lithuania data b.csv")

owls_sf_Lithuania_b <- st_as_sf(owls_Lithuania_b, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Lithuania_b)

mapview(owls_sf_Lithuania_b)

radius_Lithuania_b<-500 

owls_sf_Lithuania_b$recno<-seq(1, nrow(owls_sf_Lithuania_b),1)

buffer_500_Lithuania_b<-st_buffer(st_transform(owls_sf_Lithuania_b, "epsg:32635"),
                                  dist = radius_Lithuania_b) 

mapview(buffer_500_Lithuania_b) 

# unique(values(LC_Lithuania_b_2021))


variables_500_Lithuania_b_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_b_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_b_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_b_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_b_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_b_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Lithuania_b_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Lithuania_b), ncol = 9)) 
names(variables_500_Lithuania_b_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Lithuania_b<-st_transform(buffer_500_Lithuania_b, crs("epsg:32635"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Lithuania_b)){
  int.list_2017[[i]] <- crop(LC_Lithuania_b_2017,  buffer_500_Lithuania_b[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Lithuania_b_2018,  buffer_500_Lithuania_b[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Lithuania_b_2019,  buffer_500_Lithuania_b[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Lithuania_b_2020,  buffer_500_Lithuania_b[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Lithuania_b_2021,  buffer_500_Lithuania_b[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Lithuania_b_2022,  buffer_500_Lithuania_b[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Lithuania_b_2023,  buffer_500_Lithuania_b[i,], mask=T)
  progress(i,  nrow(buffer_500_Lithuania_b))
}

for (i in 1:nrow(buffer_500_Lithuania_b)){
  for ( j in 1:length(names(variables_500_Lithuania_b_2017))) {
    variables_500_Lithuania_b_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Lithuania_b_2017)[j])]
    variables_500_Lithuania_b_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Lithuania_b_2018)[j])]
    variables_500_Lithuania_b_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Lithuania_b_2019)[j])]
    variables_500_Lithuania_b_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Lithuania_b_2020)[j])]
    variables_500_Lithuania_b_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Lithuania_b_2021)[j])]
    variables_500_Lithuania_b_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Lithuania_b_2022)[j])]
    variables_500_Lithuania_b_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Lithuania_b_2023)[j])]
    
    progress(i,  nrow(buffer_500_Lithuania_b))
  }
}


names(variables_500_Lithuania_b_2017)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2018)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2019)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2020)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2021)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2022)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2023)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Lithuania_b_2017)
names(variables_500_Lithuania_b_2018)
names(variables_500_Lithuania_b_2019)
names(variables_500_Lithuania_b_2020)
names(variables_500_Lithuania_b_2021)
names(variables_500_Lithuania_b_2022)
names(variables_500_Lithuania_b_2023)

summary(variables_500_Lithuania_b_2017)
summary(variables_500_Lithuania_b_2018)
summary(variables_500_Lithuania_b_2019)
summary(variables_500_Lithuania_b_2020)
summary(variables_500_Lithuania_b_2021)
summary(variables_500_Lithuania_b_2022)
summary(variables_500_Lithuania_b_2023)


variables_500_Lithuania_b_2017[is.na(variables_500_Lithuania_b_2017)]<-0
variables_500_Lithuania_b_2018[is.na(variables_500_Lithuania_b_2018)]<-0
variables_500_Lithuania_b_2019[is.na(variables_500_Lithuania_b_2019)]<-0
variables_500_Lithuania_b_2020[is.na(variables_500_Lithuania_b_2020)]<-0
variables_500_Lithuania_b_2021[is.na(variables_500_Lithuania_b_2021)]<-0
variables_500_Lithuania_b_2022[is.na(variables_500_Lithuania_b_2022)]<-0
variables_500_Lithuania_b_2023[is.na(variables_500_Lithuania_b_2023)]<-0


variables_500_Lithuania_b_2017$Snow<-NULL
variables_500_Lithuania_b_2017$Clouds<-NULL

variables_500_Lithuania_b_2018$Snow<-NULL
variables_500_Lithuania_b_2018$Clouds<-NULL

variables_500_Lithuania_b_2019$Snow<-NULL
variables_500_Lithuania_b_2019$Clouds<-NULL

variables_500_Lithuania_b_2020$Snow<-NULL
variables_500_Lithuania_b_2020$Clouds<-NULL

variables_500_Lithuania_b_2021$Snow<-NULL
variables_500_Lithuania_b_2021$Clouds<-NULL

variables_500_Lithuania_b_2022$Snow<-NULL
variables_500_Lithuania_b_2022$Clouds<-NULL

variables_500_Lithuania_b_2023$Snow<-NULL
variables_500_Lithuania_b_2023$Clouds<-NULL


variables_500_Lithuania_b_2017$tot_pixel<-rowSums(variables_500_Lithuania_b_2017)
variables_500_Lithuania_b_2018$tot_pixel<-rowSums(variables_500_Lithuania_b_2018)
variables_500_Lithuania_b_2019$tot_pixel<-rowSums(variables_500_Lithuania_b_2019)
variables_500_Lithuania_b_2020$tot_pixel<-rowSums(variables_500_Lithuania_b_2020)
variables_500_Lithuania_b_2021$tot_pixel<-rowSums(variables_500_Lithuania_b_2021)
variables_500_Lithuania_b_2022$tot_pixel<-rowSums(variables_500_Lithuania_b_2022)
variables_500_Lithuania_b_2023$tot_pixel<-rowSums(variables_500_Lithuania_b_2023)



#owls_sf_Lithuania_b$Water_500<-NA
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Water/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Water/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Water/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Water/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Water/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Water/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Water_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Water/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]

owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Rangeland/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Rangeland/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Rangeland/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Rangeland/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Rangeland/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Rangeland/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Rangeland_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Rangeland/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]

owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Flood_veg/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Flood_veg/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Flood_veg/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Flood_veg/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Flood_veg/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Flood_veg/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Flood_veg_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Flood_veg/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]

owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Crops/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Crops/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Crops/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Crops/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Crops/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Crops/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Crops_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Crops/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]

owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Bare_ground/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Bare_ground/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Bare_ground/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Bare_ground/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Bare_ground/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Bare_ground/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Bare_ground_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Bare_ground/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]

owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Trees/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Trees/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Trees/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Trees/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Trees/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Trees/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Trees_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Trees/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]

owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year<=2017]<-round(variables_500_Lithuania_b_2017$Built_up/variables_500_Lithuania_b_2017$tot_pixel,3)[which(owls_sf_Lithuania_b$year<=2017)]
owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year==2018]<-round(variables_500_Lithuania_b_2018$Built_up/variables_500_Lithuania_b_2018$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2018)]
owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year==2019]<-round(variables_500_Lithuania_b_2019$Built_up/variables_500_Lithuania_b_2019$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2019)]
owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year==2020]<-round(variables_500_Lithuania_b_2020$Built_up/variables_500_Lithuania_b_2020$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2020)]
owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year==2021]<-round(variables_500_Lithuania_b_2021$Built_up/variables_500_Lithuania_b_2021$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2021)]
owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year==2022]<-round(variables_500_Lithuania_b_2022$Built_up/variables_500_Lithuania_b_2022$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2022)]
owls_sf_Lithuania_b$Built_up_500[owls_sf_Lithuania_b$year==2023]<-round(variables_500_Lithuania_b_2023$Built_up/variables_500_Lithuania_b_2023$tot_pixel,3)[which(owls_sf_Lithuania_b$year==2023)]




#################
###  NORWAY a ###
#################


LC_Norway_2017_1 <- rast("2017_Land_Cover_Norway_1.tif")
LC_Norway_2018_1 <- rast("2018_Land_Cover_Norway_1.tif")
LC_Norway_2019_1 <- rast("2019_Land_Cover_Norway_1.tif")
LC_Norway_2020_1 <- rast("2020_Land_Cover_Norway_1.tif")
LC_Norway_2021_1 <- rast("2021_Land_Cover_Norway_1.tif")
LC_Norway_2022_1 <- rast("2022_Land_Cover_Norway_1.tif")
LC_Norway_2023_1 <- rast("2023_Land_Cover_Norway_1.tif")
LC_Norway_2017_3 <- rast("2017_Land_Cover_Norway_3.tif")
LC_Norway_2018_3 <- rast("2018_Land_Cover_Norway_3.tif")
LC_Norway_2019_3 <- rast("2019_Land_Cover_Norway_3.tif")
LC_Norway_2020_3 <- rast("2020_Land_Cover_Norway_3.tif")
LC_Norway_2021_3 <- rast("2021_Land_Cover_Norway_3.tif")
LC_Norway_2022_3 <- rast("2022_Land_Cover_Norway_3.tif")
LC_Norway_2023_3 <- rast("2023_Land_Cover_Norway_3.tif")


LC_Norway_a_2017 <- merge(LC_Norway_2017_1, LC_Norway_2017_3)
writeRaster(LC_Norway_a_2017, filename = "2017_Land_Cover_Norway_a.tif")

LC_Norway_a_2018 <- merge(LC_Norway_2018_1, LC_Norway_2018_3)
writeRaster(LC_Norway_a_2018, filename = "2018_Land_Cover_Norway_a.tif")

LC_Norway_a_2019 <- merge(LC_Norway_2019_1, LC_Norway_2019_3)
writeRaster(LC_Norway_a_2019, filename = "2019_Land_Cover_Norway_a.tif")

LC_Norway_a_2020 <- merge(LC_Norway_2020_1, LC_Norway_2020_3)
writeRaster(LC_Norway_a_2020, filename = "2020_Land_Cover_Norway_a.tif")

LC_Norway_a_2021 <- merge(LC_Norway_2021_1, LC_Norway_2021_3)
writeRaster(LC_Norway_a_2021, filename = "2021_Land_Cover_Norway_a.tif")

LC_Norway_a_2022 <- merge(LC_Norway_2022_1, LC_Norway_2022_3)
writeRaster(LC_Norway_a_2022, filename = "2022_Land_Cover_Norway_a.tif")

LC_Norway_a_2023 <- merge(LC_Norway_2023_1, LC_Norway_2023_3)
writeRaster(LC_Norway_a_2023, filename = "2023_Land_Cover_Norway_a.tif")


LC_Norway_a_2017 <- rast("2017_Land_Cover_Norway_a.tif")
LC_Norway_a_2018 <- rast("2018_Land_Cover_Norway_a.tif")
LC_Norway_a_2019 <- rast("2019_Land_Cover_Norway_a.tif")
LC_Norway_a_2020 <- rast("2020_Land_Cover_Norway_a.tif")
LC_Norway_a_2021 <- rast("2021_Land_Cover_Norway_a.tif")
LC_Norway_a_2022 <- rast("2022_Land_Cover_Norway_a.tif")
LC_Norway_a_2023 <- rast("2023_Land_Cover_Norway_a.tif")



owls_Norway_a <- read.csv("Norway data a.csv")

owls_sf_Norway_a <- st_as_sf(owls_Norway_a, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Norway_a)

mapview(owls_sf_Norway_a)

radius_Norway_a<-500 

owls_sf_Norway_a$recno<-seq(1, nrow(owls_sf_Norway_a),1)

buffer_500_Norway_a<-st_buffer(st_transform(owls_sf_Norway_a, "epsg:32632"),
                                  dist = radius_Norway_a) 

mapview(buffer_500_Norway_a) 

# unique(values(LC_Norway_a_2021))


variables_500_Norway_a_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_a_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_a_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_a_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_a_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_a_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_a_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_a), ncol = 9)) 
names(variables_500_Norway_a_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Norway_a<-st_transform(buffer_500_Norway_a, crs("epsg:32632"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Norway_a)){
  int.list_2017[[i]] <- crop(LC_Norway_a_2017,  buffer_500_Norway_a[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Norway_a_2018,  buffer_500_Norway_a[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Norway_a_2019,  buffer_500_Norway_a[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Norway_a_2020,  buffer_500_Norway_a[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Norway_a_2021,  buffer_500_Norway_a[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Norway_a_2022,  buffer_500_Norway_a[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Norway_a_2023,  buffer_500_Norway_a[i,], mask=T)
  progress(i,  nrow(buffer_500_Norway_a))
}

for (i in 1:nrow(buffer_500_Norway_a)){
  for ( j in 1:length(names(variables_500_Norway_a_2017))) {
    variables_500_Norway_a_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Norway_a_2017)[j])]
    variables_500_Norway_a_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Norway_a_2018)[j])]
    variables_500_Norway_a_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Norway_a_2019)[j])]
    variables_500_Norway_a_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Norway_a_2020)[j])]
    variables_500_Norway_a_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Norway_a_2021)[j])]
    variables_500_Norway_a_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Norway_a_2022)[j])]
    variables_500_Norway_a_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Norway_a_2023)[j])]
    
    progress(i,  nrow(buffer_500_Norway_a))
  }
}


names(variables_500_Norway_a_2017)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2018)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2019)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2020)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2021)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2022)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2023)<-c("Water",
                                         "Rangeland",
                                         "Trees",
                                         "Built_up",
                                         "Snow",
                                         "Crops",
                                         "Bare_ground",
                                         "Flood_veg",
                                         "Clouds")

names(variables_500_Norway_a_2017)
names(variables_500_Norway_a_2018)
names(variables_500_Norway_a_2019)
names(variables_500_Norway_a_2020)
names(variables_500_Norway_a_2021)
names(variables_500_Norway_a_2022)
names(variables_500_Norway_a_2023)

summary(variables_500_Norway_a_2017)
summary(variables_500_Norway_a_2018)
summary(variables_500_Norway_a_2019)
summary(variables_500_Norway_a_2020)
summary(variables_500_Norway_a_2021)
summary(variables_500_Norway_a_2022)
summary(variables_500_Norway_a_2023)


variables_500_Norway_a_2017[is.na(variables_500_Norway_a_2017)]<-0
variables_500_Norway_a_2018[is.na(variables_500_Norway_a_2018)]<-0
variables_500_Norway_a_2019[is.na(variables_500_Norway_a_2019)]<-0
variables_500_Norway_a_2020[is.na(variables_500_Norway_a_2020)]<-0
variables_500_Norway_a_2021[is.na(variables_500_Norway_a_2021)]<-0
variables_500_Norway_a_2022[is.na(variables_500_Norway_a_2022)]<-0
variables_500_Norway_a_2023[is.na(variables_500_Norway_a_2023)]<-0


variables_500_Norway_a_2017$Snow<-NULL
variables_500_Norway_a_2017$Clouds<-NULL

variables_500_Norway_a_2018$Snow<-NULL
variables_500_Norway_a_2018$Clouds<-NULL

variables_500_Norway_a_2019$Snow<-NULL
variables_500_Norway_a_2019$Clouds<-NULL

variables_500_Norway_a_2020$Snow<-NULL
variables_500_Norway_a_2020$Clouds<-NULL

variables_500_Norway_a_2021$Snow<-NULL
variables_500_Norway_a_2021$Clouds<-NULL

variables_500_Norway_a_2022$Snow<-NULL
variables_500_Norway_a_2022$Clouds<-NULL

variables_500_Norway_a_2023$Snow<-NULL
variables_500_Norway_a_2023$Clouds<-NULL


variables_500_Norway_a_2017$tot_pixel<-rowSums(variables_500_Norway_a_2017)
variables_500_Norway_a_2018$tot_pixel<-rowSums(variables_500_Norway_a_2018)
variables_500_Norway_a_2019$tot_pixel<-rowSums(variables_500_Norway_a_2019)
variables_500_Norway_a_2020$tot_pixel<-rowSums(variables_500_Norway_a_2020)
variables_500_Norway_a_2021$tot_pixel<-rowSums(variables_500_Norway_a_2021)
variables_500_Norway_a_2022$tot_pixel<-rowSums(variables_500_Norway_a_2022)
variables_500_Norway_a_2023$tot_pixel<-rowSums(variables_500_Norway_a_2023)



#owls_sf_Norway_a$Water_500<-NA
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Water/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Water/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Water/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Water/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Water/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Water/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Water_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Water/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]

owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Rangeland/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Rangeland/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Rangeland/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Rangeland/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Rangeland/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Rangeland/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Rangeland_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Rangeland/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]

owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Flood_veg/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Flood_veg/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Flood_veg/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Flood_veg/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Flood_veg/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Flood_veg/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Flood_veg_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Flood_veg/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]

owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Crops/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Crops/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Crops/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Crops/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Crops/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Crops/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Crops_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Crops/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]

owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Bare_ground/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Bare_ground/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Bare_ground/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Bare_ground/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Bare_ground/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Bare_ground/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Bare_ground_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Bare_ground/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]

owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Trees/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Trees/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Trees/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Trees/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Trees/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Trees/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Trees_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Trees/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]

owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year<=2017]<-round(variables_500_Norway_a_2017$Built_up/variables_500_Norway_a_2017$tot_pixel,3)[which(owls_sf_Norway_a$year<=2017)]
owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year==2018]<-round(variables_500_Norway_a_2018$Built_up/variables_500_Norway_a_2018$tot_pixel,3)[which(owls_sf_Norway_a$year==2018)]
owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year==2019]<-round(variables_500_Norway_a_2019$Built_up/variables_500_Norway_a_2019$tot_pixel,3)[which(owls_sf_Norway_a$year==2019)]
owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year==2020]<-round(variables_500_Norway_a_2020$Built_up/variables_500_Norway_a_2020$tot_pixel,3)[which(owls_sf_Norway_a$year==2020)]
owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year==2021]<-round(variables_500_Norway_a_2021$Built_up/variables_500_Norway_a_2021$tot_pixel,3)[which(owls_sf_Norway_a$year==2021)]
owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year==2022]<-round(variables_500_Norway_a_2022$Built_up/variables_500_Norway_a_2022$tot_pixel,3)[which(owls_sf_Norway_a$year==2022)]
owls_sf_Norway_a$Built_up_500[owls_sf_Norway_a$year==2023]<-round(variables_500_Norway_a_2023$Built_up/variables_500_Norway_a_2023$tot_pixel,3)[which(owls_sf_Norway_a$year==2023)]



#################
###  NORWAY b ###
#################

LC_Norway_b_2017 <- rast("2017_Land_Cover_Norway_b.tif")
LC_Norway_b_2018 <- rast("2018_Land_Cover_Norway_b.tif")
LC_Norway_b_2019 <- rast("2019_Land_Cover_Norway_b.tif")
LC_Norway_b_2020 <- rast("2020_Land_Cover_Norway_b.tif")
LC_Norway_b_2021 <- rast("2021_Land_Cover_Norway_b.tif")
LC_Norway_b_2022 <- rast("2022_Land_Cover_Norway_b.tif")
LC_Norway_b_2023 <- rast("2023_Land_Cover_Norway_b.tif")


owls_Norway_b <- read.csv("Norway data b.csv")

owls_sf_Norway_b <- st_as_sf(owls_Norway_b, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Norway_b)

mapview(owls_sf_Norway_b)

radius_Norway_b<-500 

owls_sf_Norway_b$recno<-seq(1, nrow(owls_sf_Norway_b),1)

buffer_500_Norway_b<-st_buffer(st_transform(owls_sf_Norway_b, "epsg:32633"),
                               dist = radius_Norway_b) 

mapview(buffer_500_Norway_b) 

# unique(values(LC_Norway_b_2021))


variables_500_Norway_b_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_b_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_b_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_b_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_b_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_b_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Norway_b_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Norway_b_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Norway_b<-st_transform(buffer_500_Norway_b, crs("epsg:32633"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Norway_b)){
  int.list_2017[[i]] <- crop(LC_Norway_b_2017,  buffer_500_Norway_b[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Norway_b_2018,  buffer_500_Norway_b[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Norway_b_2019,  buffer_500_Norway_b[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Norway_b_2020,  buffer_500_Norway_b[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Norway_b_2021,  buffer_500_Norway_b[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Norway_b_2022,  buffer_500_Norway_b[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Norway_b_2023,  buffer_500_Norway_b[i,], mask=T)
  progress(i,  nrow(buffer_500_Norway_b))
}

for (i in 1:nrow(buffer_500_Norway_b)){
  for ( j in 1:length(names(variables_500_Norway_b_2017))) {
    variables_500_Norway_b_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Norway_b_2017)[j])]
    variables_500_Norway_b_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Norway_b_2018)[j])]
    variables_500_Norway_b_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Norway_b_2019)[j])]
    variables_500_Norway_b_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Norway_b_2020)[j])]
    variables_500_Norway_b_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Norway_b_2021)[j])]
    variables_500_Norway_b_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Norway_b_2022)[j])]
    variables_500_Norway_b_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Norway_b_2023)[j])]
    
    progress(i,  nrow(buffer_500_Norway_b))
  }
}

names(variables_500_Norway_b_2017)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2018)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2019)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2020)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2021)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2022)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2023)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Norway_b_2017)
names(variables_500_Norway_b_2018)
names(variables_500_Norway_b_2019)
names(variables_500_Norway_b_2020)
names(variables_500_Norway_b_2021)
names(variables_500_Norway_b_2022)
names(variables_500_Norway_b_2023)

summary(variables_500_Norway_b_2017)
summary(variables_500_Norway_b_2018)
summary(variables_500_Norway_b_2019)
summary(variables_500_Norway_b_2020)
summary(variables_500_Norway_b_2021)
summary(variables_500_Norway_b_2022)
summary(variables_500_Norway_b_2023)


variables_500_Norway_b_2017[is.na(variables_500_Norway_b_2017)]<-0
variables_500_Norway_b_2018[is.na(variables_500_Norway_b_2018)]<-0
variables_500_Norway_b_2019[is.na(variables_500_Norway_b_2019)]<-0
variables_500_Norway_b_2020[is.na(variables_500_Norway_b_2020)]<-0
variables_500_Norway_b_2021[is.na(variables_500_Norway_b_2021)]<-0
variables_500_Norway_b_2022[is.na(variables_500_Norway_b_2022)]<-0
variables_500_Norway_b_2023[is.na(variables_500_Norway_b_2023)]<-0


variables_500_Norway_b_2017$Snow<-NULL
variables_500_Norway_b_2017$Clouds<-NULL

variables_500_Norway_b_2018$Snow<-NULL
variables_500_Norway_b_2018$Clouds<-NULL

variables_500_Norway_b_2019$Snow<-NULL
variables_500_Norway_b_2019$Clouds<-NULL

variables_500_Norway_b_2020$Snow<-NULL
variables_500_Norway_b_2020$Clouds<-NULL

variables_500_Norway_b_2021$Snow<-NULL
variables_500_Norway_b_2021$Clouds<-NULL

variables_500_Norway_b_2022$Snow<-NULL
variables_500_Norway_b_2022$Clouds<-NULL

variables_500_Norway_b_2023$Snow<-NULL
variables_500_Norway_b_2023$Clouds<-NULL


variables_500_Norway_b_2017$tot_pixel<-rowSums(variables_500_Norway_b_2017)
variables_500_Norway_b_2018$tot_pixel<-rowSums(variables_500_Norway_b_2018)
variables_500_Norway_b_2019$tot_pixel<-rowSums(variables_500_Norway_b_2019)
variables_500_Norway_b_2020$tot_pixel<-rowSums(variables_500_Norway_b_2020)
variables_500_Norway_b_2021$tot_pixel<-rowSums(variables_500_Norway_b_2021)
variables_500_Norway_b_2022$tot_pixel<-rowSums(variables_500_Norway_b_2022)
variables_500_Norway_b_2023$tot_pixel<-rowSums(variables_500_Norway_b_2023)



#owls_sf_Norway_b$Water_500<-NA
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Water/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Water/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Water/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Water/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Water/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Water/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Water_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Water/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]

owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Rangeland/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Rangeland/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Rangeland/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Rangeland/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Rangeland/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Rangeland/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Rangeland_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Rangeland/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]

owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Flood_veg/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Flood_veg/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Flood_veg/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Flood_veg/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Flood_veg/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Flood_veg/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Flood_veg_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Flood_veg/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]

owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Crops/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Crops/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Crops/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Crops/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Crops/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Crops/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Crops_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Crops/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]

owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Bare_ground/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Bare_ground/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Bare_ground/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Bare_ground/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Bare_ground/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Bare_ground/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Bare_ground_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Bare_ground/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]

owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Trees/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Trees/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Trees/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Trees/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Trees/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Trees/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Trees_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Trees/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]

owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year<=2017]<-round(variables_500_Norway_b_2017$Built_up/variables_500_Norway_b_2017$tot_pixel,3)[which(owls_sf_Norway_b$year<=2017)]
owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year==2018]<-round(variables_500_Norway_b_2018$Built_up/variables_500_Norway_b_2018$tot_pixel,3)[which(owls_sf_Norway_b$year==2018)]
owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year==2019]<-round(variables_500_Norway_b_2019$Built_up/variables_500_Norway_b_2019$tot_pixel,3)[which(owls_sf_Norway_b$year==2019)]
owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year==2020]<-round(variables_500_Norway_b_2020$Built_up/variables_500_Norway_b_2020$tot_pixel,3)[which(owls_sf_Norway_b$year==2020)]
owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year==2021]<-round(variables_500_Norway_b_2021$Built_up/variables_500_Norway_b_2021$tot_pixel,3)[which(owls_sf_Norway_b$year==2021)]
owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year==2022]<-round(variables_500_Norway_b_2022$Built_up/variables_500_Norway_b_2022$tot_pixel,3)[which(owls_sf_Norway_b$year==2022)]
owls_sf_Norway_b$Built_up_500[owls_sf_Norway_b$year==2023]<-round(variables_500_Norway_b_2023$Built_up/variables_500_Norway_b_2023$tot_pixel,3)[which(owls_sf_Norway_b$year==2023)]





##################
###  FINLAND a ###
##################

LC_Finland_a_2017 <- rast("2017_Land_Cover_Finland_a.tif")
LC_Finland_a_2018 <- rast("2018_Land_Cover_Finland_a.tif")
LC_Finland_a_2019 <- rast("2019_Land_Cover_Finland_a.tif")
LC_Finland_a_2020 <- rast("2020_Land_Cover_Finland_a.tif")
LC_Finland_a_2021 <- rast("2021_Land_Cover_Finland_a.tif")
LC_Finland_a_2022 <- rast("2022_Land_Cover_Finland_a.tif")
LC_Finland_a_2023 <- rast("2023_Land_Cover_Finland_a.tif")


owls_Finland_a <- read.csv("Finland data a.csv")

owls_sf_Finland_a <- st_as_sf(owls_Finland_a, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Finland_a)

mapview(owls_sf_Finland_a)

radius_Finland_a<-500 

owls_sf_Finland_a$recno<-seq(1, nrow(owls_sf_Finland_a),1)

buffer_500_Finland_a<-st_buffer(st_transform(owls_sf_Finland_a, "epsg:32635"),
                               dist = radius_Finland_a) 

mapview(buffer_500_Finland_a) 

# unique(values(LC_Finland_a_2021))


variables_500_Finland_a_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_a_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_a_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_a_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_a_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_a_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_a_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_a), ncol = 9)) 
names(variables_500_Finland_a_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Finland_a<-st_transform(buffer_500_Finland_a, crs("epsg:32635"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Finland_a)){
  int.list_2017[[i]] <- crop(LC_Finland_a_2017,  buffer_500_Finland_a[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Finland_a_2018,  buffer_500_Finland_a[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Finland_a_2019,  buffer_500_Finland_a[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Finland_a_2020,  buffer_500_Finland_a[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Finland_a_2021,  buffer_500_Finland_a[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Finland_a_2022,  buffer_500_Finland_a[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Finland_a_2023,  buffer_500_Finland_a[i,], mask=T)
  progress(i,  nrow(buffer_500_Finland_a))
}

for (i in 1:nrow(buffer_500_Finland_a)){
  for ( j in 1:length(names(variables_500_Finland_a_2017))) {
    variables_500_Finland_a_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Finland_a_2017)[j])]
    variables_500_Finland_a_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Finland_a_2018)[j])]
    variables_500_Finland_a_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Finland_a_2019)[j])]
    variables_500_Finland_a_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Finland_a_2020)[j])]
    variables_500_Finland_a_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Finland_a_2021)[j])]
    variables_500_Finland_a_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Finland_a_2022)[j])]
    variables_500_Finland_a_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Finland_a_2023)[j])]
    
    progress(i,  nrow(buffer_500_Finland_a))
  }
}


names(variables_500_Finland_a_2017)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2018)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2019)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2020)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2021)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2022)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2023)<-c("Water",
                                      "Rangeland",
                                      "Trees",
                                      "Built_up",
                                      "Snow",
                                      "Crops",
                                      "Bare_ground",
                                      "Flood_veg",
                                      "Clouds")

names(variables_500_Finland_a_2017)
names(variables_500_Finland_a_2018)
names(variables_500_Finland_a_2019)
names(variables_500_Finland_a_2020)
names(variables_500_Finland_a_2021)
names(variables_500_Finland_a_2022)
names(variables_500_Finland_a_2023)

summary(variables_500_Finland_a_2017)
summary(variables_500_Finland_a_2018)
summary(variables_500_Finland_a_2019)
summary(variables_500_Finland_a_2020)
summary(variables_500_Finland_a_2021)
summary(variables_500_Finland_a_2022)
summary(variables_500_Finland_a_2023)


variables_500_Finland_a_2017[is.na(variables_500_Finland_a_2017)]<-0
variables_500_Finland_a_2018[is.na(variables_500_Finland_a_2018)]<-0
variables_500_Finland_a_2019[is.na(variables_500_Finland_a_2019)]<-0
variables_500_Finland_a_2020[is.na(variables_500_Finland_a_2020)]<-0
variables_500_Finland_a_2021[is.na(variables_500_Finland_a_2021)]<-0
variables_500_Finland_a_2022[is.na(variables_500_Finland_a_2022)]<-0
variables_500_Finland_a_2023[is.na(variables_500_Finland_a_2023)]<-0


variables_500_Finland_a_2017$Snow<-NULL
variables_500_Finland_a_2017$Clouds<-NULL

variables_500_Finland_a_2018$Snow<-NULL
variables_500_Finland_a_2018$Clouds<-NULL

variables_500_Finland_a_2019$Snow<-NULL
variables_500_Finland_a_2019$Clouds<-NULL

variables_500_Finland_a_2020$Snow<-NULL
variables_500_Finland_a_2020$Clouds<-NULL

variables_500_Finland_a_2021$Snow<-NULL
variables_500_Finland_a_2021$Clouds<-NULL

variables_500_Finland_a_2022$Snow<-NULL
variables_500_Finland_a_2022$Clouds<-NULL

variables_500_Finland_a_2023$Snow<-NULL
variables_500_Finland_a_2023$Clouds<-NULL


variables_500_Finland_a_2017$tot_pixel<-rowSums(variables_500_Finland_a_2017)
variables_500_Finland_a_2018$tot_pixel<-rowSums(variables_500_Finland_a_2018)
variables_500_Finland_a_2019$tot_pixel<-rowSums(variables_500_Finland_a_2019)
variables_500_Finland_a_2020$tot_pixel<-rowSums(variables_500_Finland_a_2020)
variables_500_Finland_a_2021$tot_pixel<-rowSums(variables_500_Finland_a_2021)
variables_500_Finland_a_2022$tot_pixel<-rowSums(variables_500_Finland_a_2022)
variables_500_Finland_a_2023$tot_pixel<-rowSums(variables_500_Finland_a_2023)



#owls_sf_Finland_a$Water_500<-NA
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Water/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Water/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Water/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Water/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Water/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Water/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Water_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Water/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]

owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Rangeland/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Rangeland/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Rangeland/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Rangeland/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Rangeland/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Rangeland/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Rangeland_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Rangeland/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]

owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Flood_veg/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Flood_veg/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Flood_veg/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Flood_veg/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Flood_veg/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Flood_veg/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Flood_veg_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Flood_veg/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]

owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Crops/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Crops/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Crops/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Crops/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Crops/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Crops/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Crops_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Crops/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]

owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Bare_ground/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Bare_ground/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Bare_ground/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Bare_ground/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Bare_ground/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Bare_ground/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Bare_ground_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Bare_ground/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]

owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Trees/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Trees/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Trees/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Trees/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Trees/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Trees/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Trees_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Trees/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]

owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year<=2017]<-round(variables_500_Finland_a_2017$Built_up/variables_500_Finland_a_2017$tot_pixel,3)[which(owls_sf_Finland_a$year<=2017)]
owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year==2018]<-round(variables_500_Finland_a_2018$Built_up/variables_500_Finland_a_2018$tot_pixel,3)[which(owls_sf_Finland_a$year==2018)]
owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year==2019]<-round(variables_500_Finland_a_2019$Built_up/variables_500_Finland_a_2019$tot_pixel,3)[which(owls_sf_Finland_a$year==2019)]
owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year==2020]<-round(variables_500_Finland_a_2020$Built_up/variables_500_Finland_a_2020$tot_pixel,3)[which(owls_sf_Finland_a$year==2020)]
owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year==2021]<-round(variables_500_Finland_a_2021$Built_up/variables_500_Finland_a_2021$tot_pixel,3)[which(owls_sf_Finland_a$year==2021)]
owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year==2022]<-round(variables_500_Finland_a_2022$Built_up/variables_500_Finland_a_2022$tot_pixel,3)[which(owls_sf_Finland_a$year==2022)]
owls_sf_Finland_a$Built_up_500[owls_sf_Finland_a$year==2023]<-round(variables_500_Finland_a_2023$Built_up/variables_500_Finland_a_2023$tot_pixel,3)[which(owls_sf_Finland_a$year==2023)]



##################
###  FINLAND b ###
##################

LC_Finland_b_2017 <- rast("2017_Land_Cover_Finland_b.tif")
LC_Finland_b_2018 <- rast("2018_Land_Cover_Finland_b.tif")
LC_Finland_b_2019 <- rast("2019_Land_Cover_Finland_b.tif")
LC_Finland_b_2020 <- rast("2020_Land_Cover_Finland_b.tif")
LC_Finland_b_2021 <- rast("2021_Land_Cover_Finland_b.tif")
LC_Finland_b_2022 <- rast("2022_Land_Cover_Finland_b.tif")
LC_Finland_b_2023 <- rast("2023_Land_Cover_Finland_b.tif")


owls_Finland_b <- read.csv("Finland data b.csv")

owls_sf_Finland_b <- st_as_sf(owls_Finland_b, coords = c("coordx", "coordy"), crs = 4326)
summary(owls_sf_Finland_b)

mapview(owls_sf_Finland_b)

radius_Finland_b<-500 

owls_sf_Finland_b$recno<-seq(1, nrow(owls_sf_Finland_b),1)

buffer_500_Finland_b<-st_buffer(st_transform(owls_sf_Finland_b, "epsg:32634"),
                                dist = radius_Finland_b) 

mapview(buffer_500_Finland_b) 

# unique(values(LC_Finland_b_2021))


variables_500_Finland_b_2017<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_b), ncol = 9)) 
names(variables_500_Finland_b_2017)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_b_2018<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_b), ncol = 9)) 
names(variables_500_Finland_b_2018)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_b_2019<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_b), ncol = 9)) 
names(variables_500_Finland_b_2019)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_b_2020<-data.frame(matrix(NA, nrow = nrow(owls_sf_Norway_b), ncol = 9)) 
names(variables_500_Finland_b_2020)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_b_2021<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_b), ncol = 9)) 
names(variables_500_Finland_b_2021)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_b_2022<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_b), ncol = 9)) 
names(variables_500_Finland_b_2022)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")

variables_500_Finland_b_2023<-data.frame(matrix(NA, nrow = nrow(owls_sf_Finland_b), ncol = 9)) 
names(variables_500_Finland_b_2023)<-c( "1", "11",  "2",  "7",  "9", "5",  "8",  "4", "10")


buffer_500_Finland_b<-st_transform(buffer_500_Finland_b, crs("epsg:32634"))

library(svMisc)

int.list_2017<-list()
int.list_2018<-list()
int.list_2019<-list()
int.list_2020<-list()
int.list_2021<-list()
int.list_2022<-list()
int.list_2023<-list()


for ( i in 1: nrow(buffer_500_Finland_b)){
  int.list_2017[[i]] <- crop(LC_Finland_b_2017,  buffer_500_Finland_b[i,], mask=T)
  int.list_2018[[i]] <- crop(LC_Finland_b_2018,  buffer_500_Finland_b[i,], mask=T)
  int.list_2019[[i]] <- crop(LC_Finland_b_2019,  buffer_500_Finland_b[i,], mask=T)
  int.list_2020[[i]] <- crop(LC_Finland_b_2020,  buffer_500_Finland_b[i,], mask=T)
  int.list_2021[[i]] <- crop(LC_Finland_b_2021,  buffer_500_Finland_b[i,], mask=T)
  int.list_2022[[i]] <- crop(LC_Finland_b_2022,  buffer_500_Finland_b[i,], mask=T)
  int.list_2023[[i]] <- crop(LC_Finland_b_2023,  buffer_500_Finland_b[i,], mask=T)
  progress(i,  nrow(buffer_500_Finland_b))
}

for (i in 1:nrow(buffer_500_Finland_b)){
  for ( j in 1:length(names(variables_500_Finland_b_2017))) {
    variables_500_Finland_b_2017[i,j]<-table(values(int.list_2017[[i]]))[as.character(names(variables_500_Finland_b_2017)[j])]
    variables_500_Finland_b_2018[i,j]<-table(values(int.list_2018[[i]]))[as.character(names(variables_500_Finland_b_2018)[j])]
    variables_500_Finland_b_2019[i,j]<-table(values(int.list_2019[[i]]))[as.character(names(variables_500_Finland_b_2019)[j])]
    variables_500_Finland_b_2020[i,j]<-table(values(int.list_2020[[i]]))[as.character(names(variables_500_Finland_b_2020)[j])]
    variables_500_Finland_b_2021[i,j]<-table(values(int.list_2021[[i]]))[as.character(names(variables_500_Finland_b_2021)[j])]
    variables_500_Finland_b_2022[i,j]<-table(values(int.list_2022[[i]]))[as.character(names(variables_500_Finland_b_2022)[j])]
    variables_500_Finland_b_2023[i,j]<-table(values(int.list_2023[[i]]))[as.character(names(variables_500_Finland_b_2023)[j])]
    
    progress(i,  nrow(buffer_500_Finland_b))
  }
}

names(variables_500_Finland_b_2017)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2018)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2019)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2020)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2021)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2022)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2023)<-c("Water",
                                       "Rangeland",
                                       "Trees",
                                       "Built_up",
                                       "Snow",
                                       "Crops",
                                       "Bare_ground",
                                       "Flood_veg",
                                       "Clouds")

names(variables_500_Finland_b_2017)
names(variables_500_Finland_b_2018)
names(variables_500_Finland_b_2019)
names(variables_500_Finland_b_2020)
names(variables_500_Finland_b_2021)
names(variables_500_Finland_b_2022)
names(variables_500_Finland_b_2023)

summary(variables_500_Finland_b_2017)
summary(variables_500_Finland_b_2018)
summary(variables_500_Finland_b_2019)
summary(variables_500_Finland_b_2020)
summary(variables_500_Finland_b_2021)
summary(variables_500_Finland_b_2022)
summary(variables_500_Finland_b_2023)


variables_500_Finland_b_2017[is.na(variables_500_Finland_b_2017)]<-0
variables_500_Finland_b_2018[is.na(variables_500_Finland_b_2018)]<-0
variables_500_Finland_b_2019[is.na(variables_500_Finland_b_2019)]<-0
variables_500_Finland_b_2020[is.na(variables_500_Finland_b_2020)]<-0
variables_500_Finland_b_2021[is.na(variables_500_Finland_b_2021)]<-0
variables_500_Finland_b_2022[is.na(variables_500_Finland_b_2022)]<-0
variables_500_Finland_b_2023[is.na(variables_500_Finland_b_2023)]<-0


variables_500_Finland_b_2017$Snow<-NULL
variables_500_Finland_b_2017$Clouds<-NULL

variables_500_Finland_b_2018$Snow<-NULL
variables_500_Finland_b_2018$Clouds<-NULL

variables_500_Finland_b_2019$Snow<-NULL
variables_500_Finland_b_2019$Clouds<-NULL

variables_500_Finland_b_2020$Snow<-NULL
variables_500_Finland_b_2020$Clouds<-NULL

variables_500_Finland_b_2021$Snow<-NULL
variables_500_Finland_b_2021$Clouds<-NULL

variables_500_Finland_b_2022$Snow<-NULL
variables_500_Finland_b_2022$Clouds<-NULL

variables_500_Finland_b_2023$Snow<-NULL
variables_500_Finland_b_2023$Clouds<-NULL


variables_500_Finland_b_2017$tot_pixel<-rowSums(variables_500_Finland_b_2017)
variables_500_Finland_b_2018$tot_pixel<-rowSums(variables_500_Finland_b_2018)
variables_500_Finland_b_2019$tot_pixel<-rowSums(variables_500_Finland_b_2019)
variables_500_Finland_b_2020$tot_pixel<-rowSums(variables_500_Finland_b_2020)
variables_500_Finland_b_2021$tot_pixel<-rowSums(variables_500_Finland_b_2021)
variables_500_Finland_b_2022$tot_pixel<-rowSums(variables_500_Finland_b_2022)
variables_500_Finland_b_2023$tot_pixel<-rowSums(variables_500_Finland_b_2023)



#owls_sf_Finland_a$Water_500<-NA
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Water/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Water/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Water/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Water/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Water/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Water/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Water_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Water/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]

owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Rangeland/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Rangeland/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Rangeland/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Rangeland/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Rangeland/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Rangeland/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Rangeland_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Rangeland/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]

owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Flood_veg/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Flood_veg/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Flood_veg/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Flood_veg/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Flood_veg/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Flood_veg/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Flood_veg_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Flood_veg/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]

owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Crops/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Crops/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Crops/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Crops/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Crops/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Crops/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Crops_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Crops/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]

owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Bare_ground/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Bare_ground/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Bare_ground/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Bare_ground/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Bare_ground/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Bare_ground/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Bare_ground_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Bare_ground/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]

owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Trees/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Trees/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Trees/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Trees/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Trees/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Trees/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Trees_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Trees/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]

owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year<=2017]<-round(variables_500_Finland_b_2017$Built_up/variables_500_Finland_b_2017$tot_pixel,3)[which(owls_sf_Finland_b$year<=2017)]
owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year==2018]<-round(variables_500_Finland_b_2018$Built_up/variables_500_Finland_b_2018$tot_pixel,3)[which(owls_sf_Finland_b$year==2018)]
owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year==2019]<-round(variables_500_Finland_b_2019$Built_up/variables_500_Finland_b_2019$tot_pixel,3)[which(owls_sf_Finland_b$year==2019)]
owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year==2020]<-round(variables_500_Finland_b_2020$Built_up/variables_500_Finland_b_2020$tot_pixel,3)[which(owls_sf_Finland_b$year==2020)]
owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year==2021]<-round(variables_500_Finland_b_2021$Built_up/variables_500_Finland_b_2021$tot_pixel,3)[which(owls_sf_Finland_b$year==2021)]
owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year==2022]<-round(variables_500_Finland_b_2022$Built_up/variables_500_Finland_b_2022$tot_pixel,3)[which(owls_sf_Finland_b$year==2022)]
owls_sf_Finland_b$Built_up_500[owls_sf_Finland_b$year==2023]<-round(variables_500_Finland_b_2023$Built_up/variables_500_Finland_b_2023$tot_pixel,3)[which(owls_sf_Finland_b$year==2023)]






                                      ####################################
                                      ####################################
                                      #        ROAD DATA EXTRACTION      #
                                      ####################################
                                      ####################################




################
### SCOTLAND ###
################

Roads_Scotland <- st_read("Scotland_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Scotland)
st_crs(buffer_500_Scotland)

Roads_Scotland_2<-st_transform(Roads_Scotland, st_crs(buffer_500_Scotland))
st_crs(Roads_Scotland_2)

mapview(Roads_Scotland_2)

Selection_roads_Scotland <- subset(Roads_Scotland_2, fclass=="motorway"|fclass=="motorway_link"|
                                   fclass=="primary"|fclass=="primary_link"|
                                   fclass=="trunk"|fclass=="trunk_link"|
                                   fclass=="secondary"|fclass=="seconday_link"|
                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                   fclass=="residential")

mapview(Selection_roads_Scotland)


#Total roads' length within each box buffer

Roads_in_buffer_Scotland<-st_intersection(Selection_roads_Scotland, buffer_500_Scotland)
mapview(Roads_in_buffer_Scotland)

Roads_in_buffer_Scotland$length_m<-st_length(Roads_in_buffer_Scotland)
Roads_in_buffer_Scotland

d_Scotland <- data.frame(Roads_in_buffer_Scotland)
head(d_Scotland)
aggregated_roads_Scotland<-cast(d_Scotland, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Scotland)

nrow(aggregated_roads_Scotland)

owls_sf_Scotland$ID_CODE
aggregated_roads_Scotland$ID_CODE

owls_sf_Scotland<-left_join(owls_sf_Scotland, aggregated_roads_Scotland, by="ID_CODE")
owls_sf_Scotland

owls_sf_Scotland[c(22:30)][is.na(owls_sf_Scotland[c(22:30)])] <- 0

owls_sf_Scotland <- owls_sf_Scotland %>%
  mutate(lenght_overall = rowSums(across(c(22:30))))


#Proximity to road

owls_sf_Scotland_2<-st_transform(owls_sf_Scotland, st_crs(Selection_roads_Scotland))
st_crs(owls_sf_Scotland_2)

distance.matrix.anyroad<-st_distance(owls_sf_Scotland_2, Selection_roads_Scotland)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Scotland_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Scotland_2


distance.matrix.major<-st_distance(owls_sf_Scotland_2, subset(Selection_roads_Scotland, fclass=="primary"|
                                                                fclass=="primary_link"|
                                                                fclass=="motorway"|fclass=="motorway_link"|
                                                                fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Scotland_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Scotland_2


distance.matrix.minor<-st_distance(owls_sf_Scotland_2, subset(Selection_roads_Scotland, fclass=="secondary"|
                                                                fclass=="secondary_link"|
                                                                fclass=="tertiary"|fclass=="tertiary_link"|
                                                                fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Scotland_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Scotland_2


################
###  FRANCE  ###
################

Roads_France <- st_read("France_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_France)
st_crs(buffer_500_France)

Roads_France_2<-st_transform(Roads_France, st_crs(buffer_500_France))
st_crs(Roads_France_2)

mapview(Roads_France_2)

Selection_roads_France <- subset(Roads_France_2, fclass=="motorway"|fclass=="motorway_link"|
                                     fclass=="primary"|fclass=="primary_link"|
                                     fclass=="trunk"|fclass=="trunk_link"|
                                     fclass=="secondary"|fclass=="seconday_link"|
                                     fclass=="tertiary"|fclass=="tertiary_link"|
                                     fclass=="residential")

mapview(Selection_roads_France)


#Total roads' lenght within each box buffer

Roads_in_buffer_France<-st_intersection(Selection_roads_France, buffer_500_France)
mapview(Roads_in_buffer_France)

Roads_in_buffer_France$length_m<-st_length(Roads_in_buffer_France)
Roads_in_buffer_France

d_France <- data.frame(Roads_in_buffer_France)
head(d_France)
aggregated_roads_France<-cast(d_France, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_France)

nrow(aggregated_roads_France)

owls_sf_France$ID_CODE
aggregated_roads_France$ID_CODE

owls_sf_France<-left_join(owls_sf_France, aggregated_roads_France, by="ID_CODE")
owls_sf_France

owls_sf_France[c(22:28)][is.na(owls_sf_France[c(22:28)])]<-0

owls_sf_France <- owls_sf_France %>%
  mutate(lenght_overall = rowSums(across(c(22:28))))


#Proximity to road

owls_sf_France_2<-st_transform(owls_sf_France, st_crs(Selection_roads_France))
st_crs(owls_sf_France_2)

distance.matrix.anyroad<-st_distance(owls_sf_France_2, Selection_roads_France)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_France_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_France_2


distance.matrix.major<-st_distance(owls_sf_France_2, subset(Selection_roads_France, fclass=="primary"|
                                                                fclass=="primary_link"|
                                                                fclass=="motorway"|fclass=="motorway_link"|
                                                              fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_France_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_France_2


distance.matrix.minor<-st_distance(owls_sf_France_2, subset(Selection_roads_France, fclass=="secondary"|
                                                                fclass=="secondary_link"|
                                                                fclass=="tertiary"|fclass=="tertiary_link"|
                                                              fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_France_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_France_2


################
###  SWEDEN  ###
################

Roads_Sweden <- st_read("Sweden_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Sweden)
st_crs(buffer_500_Sweden)

Roads_Sweden_2<-st_transform(Roads_Sweden, st_crs(buffer_500_Sweden))
st_crs(Roads_Sweden_2)

mapview(Roads_Sweden_2)

Selection_roads_Sweden <- subset(Roads_Sweden_2, fclass=="motorway"|fclass=="motorway_link"|
                                   fclass=="primary"|fclass=="primary_link"|
                                   fclass=="trunk"|fclass=="trunk_link"|
                                   fclass=="secondary"|fclass=="seconday_link"|
                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                   fclass=="residential")

mapview(Selection_roads_Sweden)


#Total roads' length within each box buffer

Roads_in_buffer_Sweden<-st_intersection(Selection_roads_Sweden, buffer_500_Sweden)
mapview(Roads_in_buffer_Sweden)

Roads_in_buffer_Sweden$length_m<-st_length(Roads_in_buffer_Sweden)
Roads_in_buffer_Sweden

d_Sweden <- data.frame(Roads_in_buffer_Sweden)
head(d_Sweden)
aggregated_roads_Sweden<-cast(d_Sweden, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Sweden)

nrow(aggregated_roads_Sweden)

owls_sf_Sweden$ID_CODE
aggregated_roads_Sweden$ID_CODE

owls_sf_Sweden<-left_join(owls_sf_Sweden, aggregated_roads_Sweden, by="ID_CODE")
owls_sf_Sweden

owls_sf_Sweden[c(22:25)][is.na(owls_sf_Sweden[c(22:25)])]<-0

owls_sf_Sweden <- owls_sf_Sweden %>%
  mutate(lenght_overall = rowSums(across(c(22:25))))


#Proximity to road

owls_sf_Sweden_2<-st_transform(owls_sf_Sweden, st_crs(Selection_roads_Sweden))
st_crs(owls_sf_Sweden_2)

distance.matrix.anyroad<-st_distance(owls_sf_Sweden_2, Selection_roads_Sweden)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Sweden_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Sweden_2


distance.matrix.major<-st_distance(owls_sf_Sweden_2, subset(Selection_roads_Sweden, fclass=="primary"|
                                                              fclass=="primary_link"|
                                                              fclass=="motorway"|fclass=="motorway_link"|
                                                              fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Sweden_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Sweden_2


distance.matrix.minor<-st_distance(owls_sf_Sweden_2, subset(Selection_roads_Sweden, fclass=="secondary"|
                                                              fclass=="secondary_link"|
                                                              fclass=="tertiary"|fclass=="tertiary_link"|
                                                              fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Sweden_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Sweden_2


##################
###  SLOVENIA  ###
##################

Roads_Slovenia <- st_read("Slovenia_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Slovenia)
st_crs(buffer_500_Slovenia)

Roads_Slovenia_2<-st_transform(Roads_Slovenia, st_crs(buffer_500_Slovenia))
st_crs(Roads_Slovenia_2)

mapview(Roads_Slovenia_2)

Selection_roads_Slovenia <- subset(Roads_Slovenia_2, fclass=="motorway"|fclass=="motorway_link"|
                                   fclass=="primary"|fclass=="primary_link"|
                                   fclass=="trunk"|fclass=="trunk_link"|
                                   fclass=="secondary"|fclass=="seconday_link"|
                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                   fclass=="residential")

mapview(Selection_roads_Slovenia)


#Total roads' length within each box buffer

Roads_in_buffer_Slovenia<-st_intersection(Selection_roads_Slovenia, buffer_500_Slovenia)
mapview(Roads_in_buffer_Slovenia)

Roads_in_buffer_Slovenia$length_m<-st_length(Roads_in_buffer_Slovenia)
Roads_in_buffer_Slovenia

d_Slovenia <- data.frame(Roads_in_buffer_Slovenia)
head(d_Slovenia)
aggregated_roads_Slovenia<-cast(d_Slovenia, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Slovenia)

nrow(aggregated_roads_Slovenia)

owls_sf_Slovenia$ID_CODE
aggregated_roads_Slovenia$ID_CODE

owls_sf_Slovenia<-left_join(owls_sf_Slovenia, aggregated_roads_Slovenia, by="ID_CODE")
owls_sf_Slovenia

owls_sf_Slovenia[c(22:26)][is.na(owls_sf_Slovenia[c(22:26)])]<-0

owls_sf_Slovenia <- owls_sf_Slovenia %>%
  mutate(lenght_overall = rowSums(across(c(22:26))))


#Proximity to road

owls_sf_Slovenia_2<-st_transform(owls_sf_Slovenia, st_crs(Selection_roads_Slovenia))
st_crs(owls_sf_Slovenia_2)

distance.matrix.anyroad<-st_distance(owls_sf_Slovenia_2, Selection_roads_Slovenia)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Slovenia_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Slovenia_2


distance.matrix.major<-st_distance(owls_sf_Slovenia_2, subset(Selection_roads_Slovenia, fclass=="primary"|
                                                              fclass=="primary_link"|
                                                              fclass=="motorway"|fclass=="motorway_link"|
                                                              fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Slovenia_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Slovenia_2


distance.matrix.minor<-st_distance(owls_sf_Slovenia_2, subset(Selection_roads_Slovenia, fclass=="secondary"|
                                                              fclass=="secondary_link"|
                                                              fclass=="tertiary"|fclass=="tertiary_link"|
                                                              fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Slovenia_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Slovenia_2


#####################
###  SWITZERLAND  ###
#####################

Roads_Switzerland <- st_read("Switzerland_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Switzerland)
st_crs(buffer_500_Switzerland)

Roads_Switzerland_2<-st_transform(Roads_Switzerland, st_crs(buffer_500_Switzerland))
st_crs(Roads_Switzerland_2)

mapview(Roads_Switzerland_2)

Selection_roads_Switzerland <- subset(Roads_Switzerland_2, fclass=="motorway"|fclass=="motorway_link"|
                                     fclass=="primary"|fclass=="primary_link"|
                                     fclass=="trunk"|fclass=="trunk_link"|
                                     fclass=="secondary"|fclass=="seconday_link"|
                                     fclass=="tertiary"|fclass=="tertiary_link"|
                                     fclass=="residential")

mapview(Selection_roads_Switzerland)


#Total roads' length within each box buffer

Roads_in_buffer_Switzerland<-st_intersection(Selection_roads_Switzerland, buffer_500_Switzerland)
mapview(Roads_in_buffer_Switzerland)

Roads_in_buffer_Switzerland$length_m<-st_length(Roads_in_buffer_Switzerland)
Roads_in_buffer_Switzerland

d_Switzerland <- data.frame(Roads_in_buffer_Switzerland)
head(d_Switzerland)
aggregated_roads_Switzerland<-cast(d_Switzerland, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Switzerland)

nrow(aggregated_roads_Switzerland)

owls_sf_Switzerland$ID_CODE
aggregated_roads_Switzerland$ID_CODE

owls_sf_Switzerland<-left_join(owls_sf_Switzerland, aggregated_roads_Switzerland, by="ID_CODE")
owls_sf_Switzerland

owls_sf_Switzerland[c(23:30)][is.na(owls_sf_Switzerland[c(23:30)])]<-0

owls_sf_Switzerland <- owls_sf_Switzerland %>%
  mutate(lenght_overall = rowSums(across(c(23:30))))


#Proximity to road

owls_sf_Switzerland_2<-st_transform(owls_sf_Switzerland, st_crs(Selection_roads_Switzerland))
st_crs(owls_sf_Switzerland_2)

distance.matrix.anyroad<-st_distance(owls_sf_Switzerland_2, Selection_roads_Switzerland)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Switzerland_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Switzerland_2


distance.matrix.major<-st_distance(owls_sf_Switzerland_2, subset(Selection_roads_Switzerland, fclass=="primary"|
                                                                fclass=="primary_link"|
                                                                fclass=="motorway"|fclass=="motorway_link"|
                                                                fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Switzerland_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Switzerland_2


distance.matrix.minor<-st_distance(owls_sf_Switzerland_2, subset(Selection_roads_Switzerland, fclass=="secondary"|
                                                                fclass=="secondary_link"|
                                                                fclass=="tertiary"|fclass=="tertiary_link"|
                                                                fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Switzerland_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Switzerland_2


################
###  CZECHIA ###
################

Roads_Czechia <- st_read("Czechia_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Czechia)
st_crs(buffer_500_Czechia)

Roads_Czechia_2<-st_transform(Roads_Czechia, st_crs(buffer_500_Czechia))
st_crs(Roads_Czechia_2)

mapview(Roads_Czechia_2)

Selection_roads_Czechia <- subset(Roads_Czechia_2, fclass=="motorway"|fclass=="motorway_link"|
                                        fclass=="primary"|fclass=="primary_link"|
                                        fclass=="trunk"|fclass=="trunk_link"|
                                        fclass=="secondary"|fclass=="seconday_link"|
                                        fclass=="tertiary"|fclass=="tertiary_link"|
                                        fclass=="residential")

mapview(Selection_roads_Czechia)


#Total roads' length within each box buffer

Roads_in_buffer_Czechia<-st_intersection(Selection_roads_Czechia, buffer_500_Czechia)
mapview(Roads_in_buffer_Czechia)

Roads_in_buffer_Czechia$length_m<-st_length(Roads_in_buffer_Czechia)
Roads_in_buffer_Czechia

d_Czechia <- data.frame(Roads_in_buffer_Czechia)
head(d_Czechia)
aggregated_roads_Czechia<-cast(d_Czechia, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Czechia)

nrow(aggregated_roads_Czechia)

owls_sf_Czechia$ID_CODE
aggregated_roads_Czechia$ID_CODE

owls_sf_Czechia<-left_join(owls_sf_Czechia, aggregated_roads_Czechia, by="ID_CODE")
owls_sf_Czechia

owls_sf_Czechia[c(22:30)][is.na(owls_sf_Czechia[c(22:30)])]<-0

owls_sf_Czechia <- owls_sf_Czechia %>%
  mutate(lenght_overall = rowSums(across(c(22:30))))


#Proximity to road

owls_sf_Czechia_2<-st_transform(owls_sf_Czechia, st_crs(Selection_roads_Czechia))
st_crs(owls_sf_Czechia_2)

distance.matrix.anyroad<-st_distance(owls_sf_Czechia_2, Selection_roads_Czechia)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Czechia_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Czechia_2


distance.matrix.major<-st_distance(owls_sf_Czechia_2, subset(Selection_roads_Czechia, fclass=="primary"|
                                                                   fclass=="primary_link"|
                                                                   fclass=="motorway"|fclass=="motorway_link"|
                                                                   fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Czechia_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Czechia_2


distance.matrix.minor<-st_distance(owls_sf_Czechia_2, subset(Selection_roads_Czechia, fclass=="secondary"|
                                                                   fclass=="secondary_link"|
                                                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                                                   fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Czechia_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Czechia_2


####################
###  LITHUANIA a ###
####################

Roads_Lithuania_a <- st_read("Lithuania_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Lithuania_a)
st_crs(buffer_500_Lithuania_a)

Roads_Lithuania_a_2<-st_transform(Roads_Lithuania_a, st_crs(buffer_500_Lithuania_a))
st_crs(Roads_Lithuania_a_2)

mapview(Roads_Lithuania_a_2)

Selection_roads_Lithuania_a <- subset(Roads_Lithuania_a_2, fclass=="motorway"|fclass=="motorway_link"|
                                    fclass=="primary"|fclass=="primary_link"|
                                    fclass=="trunk"|fclass=="trunk_link"|
                                    fclass=="secondary"|fclass=="seconday_link"|
                                    fclass=="tertiary"|fclass=="tertiary_link"|
                                    fclass=="residential")

mapview(Selection_roads_Lithuania_a)


#Total roads' length within each box buffer

Roads_in_buffer_Lithuania_a<-st_intersection(Selection_roads_Lithuania_a, buffer_500_Lithuania_a)
mapview(Roads_in_buffer_Lithuania_a)

Roads_in_buffer_Lithuania_a$length_m<-st_length(Roads_in_buffer_Lithuania_a)
Roads_in_buffer_Lithuania_a

d_Lithuania_a <- data.frame(Roads_in_buffer_Lithuania_a)
head(d_Lithuania_a)
aggregated_roads_Lithuania_a<-cast(d_Lithuania_a, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Lithuania_a)

nrow(aggregated_roads_Lithuania_a)

owls_sf_Lithuania_a$ID_CODE
aggregated_roads_Lithuania_a$ID_CODE

owls_sf_Lithuania_a<-left_join(owls_sf_Lithuania_a, aggregated_roads_Lithuania_a, by="ID_CODE")
owls_sf_Lithuania_a

owls_sf_Lithuania_a[c(22:24)][is.na(owls_sf_Lithuania_a[c(22:24)])]<-0

owls_sf_Lithuania_a <- owls_sf_Lithuania_a %>%
  mutate(lenght_overall = rowSums(across(c(22:24))))


#Proximity to road

owls_sf_Lithuania_a_2<-st_transform(owls_sf_Lithuania_a, st_crs(Selection_roads_Lithuania_a))
st_crs(owls_sf_Lithuania_a_2)

distance.matrix.anyroad<-st_distance(owls_sf_Lithuania_a_2, Selection_roads_Lithuania_a)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Lithuania_a_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Lithuania_a_2


distance.matrix.major<-st_distance(owls_sf_Lithuania_a_2, subset(Selection_roads_Lithuania_a, fclass=="primary"|
                                                               fclass=="primary_link"|
                                                               fclass=="motorway"|fclass=="motorway_link"|
                                                               fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Lithuania_a_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Lithuania_a_2


distance.matrix.minor<-st_distance(owls_sf_Lithuania_a_2, subset(Selection_roads_Lithuania_a, fclass=="secondary"|
                                                                   fclass=="secondary_link"|
                                                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                                                   fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Lithuania_a_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Lithuania_a_2


####################
###  LITHUANIA b ###
####################

Roads_Lithuania_b <- st_read("Lithuania_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Lithuania_b)
st_crs(buffer_500_Lithuania_b)

Roads_Lithuania_b_2<-st_transform(Roads_Lithuania_b, st_crs(buffer_500_Lithuania_b))
st_crs(Roads_Lithuania_b_2)

mapview(Roads_Lithuania_b_2)

Selection_roads_Lithuania_b <- subset(Roads_Lithuania_b_2, fclass=="motorway"|fclass=="motorway_link"|
                                        fclass=="primary"|fclass=="primary_link"|
                                        fclass=="trunk"|fclass=="trunk_link"|
                                        fclass=="secondary"|fclass=="seconday_link"|
                                        fclass=="tertiary"|fclass=="tertiary_link"|
                                        fclass=="residential")

mapview(Selection_roads_Lithuania_b)


#Total roads' length within each box buffer

Roads_in_buffer_Lithuania_b<-st_intersection(Selection_roads_Lithuania_b, buffer_500_Lithuania_b)
mapview(Roads_in_buffer_Lithuania_b)

Roads_in_buffer_Lithuania_b$length_m<-st_length(Roads_in_buffer_Lithuania_b)
Roads_in_buffer_Lithuania_b

d_Lithuania_b <- data.frame(Roads_in_buffer_Lithuania_b)
head(d_Lithuania_b)
aggregated_roads_Lithuania_b<-cast(d_Lithuania_b, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Lithuania_b)

nrow(aggregated_roads_Lithuania_b)

owls_sf_Lithuania_b$ID_CODE
aggregated_roads_Lithuania_b$ID_CODE

owls_sf_Lithuania_b<-left_join(owls_sf_Lithuania_b, aggregated_roads_Lithuania_b, by="ID_CODE")
owls_sf_Lithuania_b

owls_sf_Lithuania_b[c(22:26)][is.na(owls_sf_Lithuania_b[c(22:26)])]<-0

owls_sf_Lithuania_b <- owls_sf_Lithuania_b %>%
  mutate(lenght_overall = rowSums(across(c(22:26))))


#Proximity to road

owls_sf_Lithuania_b_2<-st_transform(owls_sf_Lithuania_b, st_crs(Selection_roads_Lithuania_b))
st_crs(owls_sf_Lithuania_b_2)

distance.matrix.anyroad<-st_distance(owls_sf_Lithuania_b_2, Selection_roads_Lithuania_b)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Lithuania_b_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Lithuania_b_2


distance.matrix.major<-st_distance(owls_sf_Lithuania_b_2, subset(Selection_roads_Lithuania_b, fclass=="primary"|
                                                                   fclass=="primary_link"|
                                                                   fclass=="motorway"|fclass=="motorway_link"|
                                                                   fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Lithuania_b_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Lithuania_b_2


distance.matrix.minor<-st_distance(owls_sf_Lithuania_b_2, subset(Selection_roads_Lithuania_b, fclass=="secondary"|
                                                                   fclass=="secondary_link"|
                                                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                                                   fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Lithuania_b_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Lithuania_b_2



#################
###  NORWAY a ###
#################

Roads_Norway_a <- st_read("Norway_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Norway_a)
st_crs(buffer_500_Norway_a)

Roads_Norway_a_2<-st_transform(Roads_Norway_a, st_crs(buffer_500_Norway_a))
st_crs(Roads_Norway_a_2)

mapview(Roads_Norway_a_2)

Selection_roads_Norway_a <- subset(Roads_Norway_a_2, fclass=="motorway"|fclass=="motorway_link"|
                                        fclass=="primary"|fclass=="primary_link"|
                                        fclass=="trunk"|fclass=="trunk_link"|
                                        fclass=="secondary"|fclass=="seconday_link"|
                                        fclass=="tertiary"|fclass=="tertiary_link"|
                                        fclass=="residential")

mapview(Selection_roads_Norway_a)


#Total roads' length within each box buffer

Roads_in_buffer_Norway_a<-st_intersection(Selection_roads_Norway_a, buffer_500_Norway_a)
mapview(Roads_in_buffer_Norway_a)

Roads_in_buffer_Norway_a$length_m<-st_length(Roads_in_buffer_Norway_a)
Roads_in_buffer_Norway_a

d_Norway_a <- data.frame(Roads_in_buffer_Norway_a)
head(d_Norway_a)
aggregated_roads_Norway_a<-cast(d_Norway_a, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Norway_a)

nrow(aggregated_roads_Norway_a)

owls_sf_Norway_a$ID_CODE
aggregated_roads_Norway_a$ID_CODE

owls_sf_Norway_a<-left_join(owls_sf_Norway_a, aggregated_roads_Norway_a, by="ID_CODE")
owls_sf_Norway_a

owls_sf_Norway_a[c(22:27)][is.na(owls_sf_Norway_a[c(22:27)])]<-0

owls_sf_Norway_a <- owls_sf_Norway_a %>%
  mutate(lenght_overall = rowSums(across(c(22:27))))


#Proximity to road

owls_sf_Norway_a_2<-st_transform(owls_sf_Norway_a, st_crs(Selection_roads_Norway_a))
st_crs(owls_sf_Norway_a_2)

distance.matrix.anyroad<-st_distance(owls_sf_Norway_a_2, Selection_roads_Norway_a)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Norway_a_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Norway_a_2


distance.matrix.major<-st_distance(owls_sf_Norway_a_2, subset(Selection_roads_Norway_a, fclass=="primary"|
                                                                   fclass=="primary_link"|
                                                                   fclass=="motorway"|fclass=="motorway_link"|
                                                                   fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Norway_a_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Norway_a_2


distance.matrix.minor<-st_distance(owls_sf_Norway_a_2, subset(Selection_roads_Norway_a, fclass=="secondary"|
                                                                   fclass=="secondary_link"|
                                                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                                                   fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Norway_a_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Norway_a_2


#################
###  NORWAY b ###
#################

Roads_Norway_b <- st_read("Norway_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Norway_b)
st_crs(buffer_500_Norway_b)

Roads_Norway_b_2<-st_transform(Roads_Norway_b, st_crs(buffer_500_Norway_b))
st_crs(Roads_Norway_b_2)

mapview(Roads_Norway_b_2)

Selection_roads_Norway_b <- subset(Roads_Norway_b_2, fclass=="motorway"|fclass=="motorway_link"|
                                        fclass=="primary"|fclass=="primary_link"|
                                        fclass=="trunk"|fclass=="trunk_link"|
                                        fclass=="secondary"|fclass=="seconday_link"|
                                        fclass=="tertiary"|fclass=="tertiary_link"|
                                        fclass=="residential")

mapview(Selection_roads_Norway_b)


#Total roads' length within each box buffer

Roads_in_buffer_Norway_b<-st_intersection(Selection_roads_Norway_b, buffer_500_Norway_b)
mapview(Roads_in_buffer_Norway_b)

Roads_in_buffer_Norway_b$length_m<-st_length(Roads_in_buffer_Norway_b)
Roads_in_buffer_Norway_b

d_Norway_b <- data.frame(Roads_in_buffer_Norway_b)
head(d_Norway_b)
aggregated_roads_Norway_b<-cast(d_Norway_b, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Norway_b)

nrow(aggregated_roads_Norway_b)

owls_sf_Norway_b$ID_CODE
aggregated_roads_Norway_b$ID_CODE

owls_sf_Norway_b<-left_join(owls_sf_Norway_b, aggregated_roads_Norway_b, by="ID_CODE")
owls_sf_Norway_b

owls_sf_Norway_b[c(22:26)][is.na(owls_sf_Norway_b[c(22:26)])]<-0

owls_sf_Norway_b <- owls_sf_Norway_b %>%
  mutate(lenght_overall = rowSums(across(c(22:26))))


#Proximity to road

owls_sf_Norway_b_2<-st_transform(owls_sf_Norway_b, st_crs(Selection_roads_Norway_b))
st_crs(owls_sf_Norway_b_2)

distance.matrix.anyroad<-st_distance(owls_sf_Norway_b_2, Selection_roads_Norway_b)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Norway_b_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Norway_b_2


distance.matrix.major<-st_distance(owls_sf_Norway_b_2, subset(Selection_roads_Norway_b, fclass=="primary"|
                                                                   fclass=="primary_link"|
                                                                   fclass=="motorway"|fclass=="motorway_link"|
                                                                   fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Norway_b_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Norway_b_2


distance.matrix.minor<-st_distance(owls_sf_Norway_b_2, subset(Selection_roads_Norway_b, fclass=="secondary"|
                                                                   fclass=="secondary_link"|
                                                                   fclass=="tertiary"|fclass=="tertiary_link"|
                                                                   fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Norway_b_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Norway_b_2


##################
###  FINLAND a ###
##################

Roads_Finland_a <- st_read("Finland_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Finland_a)
st_crs(buffer_500_Finland_a)

Roads_Finland_a_2<-st_transform(Roads_Finland_a, st_crs(buffer_500_Finland_a))
st_crs(Roads_Finland_a_2)

mapview(Roads_Finland_a_2)

Selection_roads_Finland_a <- subset(Roads_Finland_a_2, fclass=="motorway"|fclass=="motorway_link"|
                                     fclass=="primary"|fclass=="primary_link"|
                                     fclass=="trunk"|fclass=="trunk_link"|
                                     fclass=="secondary"|fclass=="seconday_link"|
                                     fclass=="tertiary"|fclass=="tertiary_link"|
                                     fclass=="residential")

mapview(Selection_roads_Finland_a)


#Total roads' length within each box buffer

Roads_in_buffer_Finland_a<-st_intersection(Selection_roads_Finland_a, buffer_500_Finland_a)
mapview(Roads_in_buffer_Finland_a)

Roads_in_buffer_Finland_a$length_m<-st_length(Roads_in_buffer_Finland_a)
Roads_in_buffer_Finland_a

d_Finland_a <- data.frame(Roads_in_buffer_Finland_a)
head(d_Finland_a)
aggregated_roads_Finland_a<-cast(d_Finland_a, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Finland_a)

nrow(aggregated_roads_Finland_a)

owls_sf_Finland_a$ID_CODE
aggregated_roads_Finland_a$ID_CODE

owls_sf_Finland_a<-left_join(owls_sf_Finland_a, aggregated_roads_Finland_a, by="ID_CODE")
owls_sf_Finland_a

owls_sf_Finland_a[c(22:28)][is.na(owls_sf_Finland_a[c(22:28)])]<-0

owls_sf_Finland_a <- owls_sf_Finland_a %>%
  mutate(lenght_overall = rowSums(across(c(22:28))))


#Proximity to road

owls_sf_Finland_a_2<-st_transform(owls_sf_Finland_a, st_crs(Selection_roads_Finland_a))
st_crs(owls_sf_Finland_a_2)

distance.matrix.anyroad<-st_distance(owls_sf_Finland_a_2, Selection_roads_Finland_a)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Finland_a_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Finland_a_2


distance.matrix.major<-st_distance(owls_sf_Finland_a_2, subset(Selection_roads_Finland_a, fclass=="primary"|
                                                                fclass=="primary_link"|
                                                                fclass=="motorway"|fclass=="motorway_link"|
                                                                fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Finland_a_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Finland_a_2


distance.matrix.minor<-st_distance(owls_sf_Finland_a_2, subset(Selection_roads_Finland_a, fclass=="secondary"|
                                                                fclass=="secondary_link"|
                                                                fclass=="tertiary"|fclass=="tertiary_link"|
                                                                fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Finland_a_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Finland_a_2



##################
###  FINLAND b ###
##################

Roads_Finland_b <- st_read("Finland_gis_osm_roads_free_1_crop.shp")

st_crs(Roads_Finland_b)
st_crs(buffer_500_Finland_b)

Roads_Finland_b_2<-st_transform(Roads_Finland_b, st_crs(buffer_500_Finland_b))
st_crs(Roads_Finland_b_2)

mapview(Roads_Finland_b_2)

Selection_roads_Finland_b <- subset(Roads_Finland_b_2, fclass=="motorway"|fclass=="motorway_link"|
                                      fclass=="primary"|fclass=="primary_link"|
                                      fclass=="trunk"|fclass=="trunk_link"|
                                      fclass=="secondary"|fclass=="seconday_link"|
                                      fclass=="tertiary"|fclass=="tertiary_link"|
                                      fclass=="residential")

mapview(Selection_roads_Finland_b)


#Total roads' length within each box buffer

Roads_in_buffer_Finland_b<-st_intersection(Selection_roads_Finland_b, buffer_500_Finland_b)
mapview(Roads_in_buffer_Finland_b)

Roads_in_buffer_Finland_b$length_m<-st_length(Roads_in_buffer_Finland_b)
Roads_in_buffer_Finland_b

d_Finland_b <- data.frame(Roads_in_buffer_Finland_b)
head(d_Finland_b)
aggregated_roads_Finland_b<-cast(d_Finland_b, ID_CODE ~fclass , value="length_m", fun.aggregate=sum)
head(aggregated_roads_Finland_b)

nrow(aggregated_roads_Finland_b)

owls_sf_Finland_b$ID_CODE
aggregated_roads_Finland_b$ID_CODE

owls_sf_Finland_b<-left_join(owls_sf_Finland_b, aggregated_roads_Finland_b, by="ID_CODE")
owls_sf_Finland_b

owls_sf_Finland_b[c(22:27)][is.na(owls_sf_Finland_b[c(22:27)])]<-0

owls_sf_Finland_b <- owls_sf_Finland_b %>%
  mutate(lenght_overall = rowSums(across(c(22:27))))


#Proximity to road

owls_sf_Finland_b_2<-st_transform(owls_sf_Finland_b, st_crs(Selection_roads_Finland_b))
st_crs(owls_sf_Finland_b_2)

distance.matrix.anyroad<-st_distance(owls_sf_Finland_b_2, Selection_roads_Finland_b)
distance.matrix.anyroad
dim(distance.matrix.anyroad)
owls_sf_Finland_b_2$D_nearest_anyroad<-apply(distance.matrix.anyroad,1,min)
owls_sf_Finland_b_2


distance.matrix.major<-st_distance(owls_sf_Finland_b_2, subset(Selection_roads_Finland_b, fclass=="primary"|
                                                                 fclass=="primary_link"|
                                                                 fclass=="motorway"|fclass=="motorway_link"|
                                                                 fclass=="trunk"|fclass=="trunk_link"))
distance.matrix.major
dim(distance.matrix.major)
owls_sf_Finland_b_2$D_nearest_major<-apply(distance.matrix.major,1,min)
owls_sf_Finland_b_2


distance.matrix.minor<-st_distance(owls_sf_Finland_b_2, subset(Selection_roads_Finland_b, fclass=="secondary"|
                                                                 fclass=="secondary_link"|
                                                                 fclass=="tertiary"|fclass=="tertiary_link"|
                                                                 fclass=="residential"))
distance.matrix.minor
dim(distance.matrix.minor)
owls_sf_Finland_b_2$D_nearest_minor<-apply(distance.matrix.minor,1,min)
owls_sf_Finland_b_2





                                                ####################################
                                                ####################################
                                                #     BUILDING DATA EXTRACTION     #
                                                ####################################
                                                ####################################

################
### SCOTLAND ###
################

Buildings_Scotland <- st_read("Scotland_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Scotland)
st_crs(buffer_500_Scotland)

Buildings_Scotland_2<-st_transform(Buildings_Scotland, st_crs(buffer_500_Scotland))
st_crs(Buildings_Scotland_2)

mapview(Buildings_Scotland_2) 

mapview(owls_sf_Scotland)


#Total buildings within each box buffer

Buildings_in_buffer_Scotland<-st_intersection(Buildings_Scotland_2, buffer_500_Scotland)
mapview(Buildings_in_buffer_Scotland)

Buildings_in_buffer_Scotland$area_m2<-st_area(Buildings_in_buffer_Scotland)
Buildings_in_buffer_Scotland

d_buildings_Scotland <- data.frame(Buildings_in_buffer_Scotland)
head(d_buildings_Scotland)
aggregated_buildings_Scotland<-cast(d_buildings_Scotland, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Scotland_2)

nrow(aggregated_buildings_Scotland)


owls_sf_Scotland_2$ID_CODE
aggregated_buildings_Scotland$ID_CODE

owls_sf_Scotland_2<-left_join(owls_sf_Scotland_2, aggregated_buildings_Scotland, by="ID_CODE")
owls_sf_Scotland_2

owls_sf_Scotland_2[c(45)][is.na(owls_sf_Scotland_2[c(45)])] <- 0


owls_sf_Scotland_2$buildings_prop <- (owls_sf_Scotland_2$building/(500*500*3.14))*100


owls_sf_Scotland_2[c(47)][is.na(owls_sf_Scotland_2[c(47)])] <- 0



##############
### FRANCE ###
##############

Buildings_France <- st_read("France_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_France)
st_crs(buffer_500_France)

Buildings_France_2<-st_transform(Buildings_France, st_crs(buffer_500_France))
st_crs(Buildings_France_2)

mapview(Buildings_France_2) 

mapview(owls_sf_France)


#Total buildings within each box buffer

Buildings_in_buffer_France<-st_intersection(Buildings_France_2, buffer_500_France)
mapview(Buildings_in_buffer_France)

Buildings_in_buffer_France$area_m2<-st_area(Buildings_in_buffer_France)
Buildings_in_buffer_France

d_buildings_France <- data.frame(Buildings_in_buffer_France)
head(d_buildings_France)
aggregated_buildings_France<-cast(d_buildings_France, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_France_2)

nrow(aggregated_buildings_France)


owls_sf_France_2$ID_CODE
aggregated_buildings_France$ID_CODE

owls_sf_France_2<-left_join(owls_sf_France_2, aggregated_buildings_France, by="ID_CODE")
owls_sf_France_2

owls_sf_France_2[c(43)][is.na(owls_sf_France_2[c(43)])] <- 0


owls_sf_France_2$buildings_prop <- (owls_sf_France_2$building/(500*500*3.14))*100


owls_sf_France_2[c(45)][is.na(owls_sf_France_2[c(45)])] <- 0


##############
### SWEDEN ###
##############

Buildings_Sweden <- st_read("Sweden_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Sweden)
st_crs(buffer_500_Sweden)

Buildings_Sweden_2<-st_transform(Buildings_Sweden, st_crs(buffer_500_Sweden))
st_crs(Buildings_Sweden_2)

mapview(Buildings_Sweden_2) 

mapview(owls_sf_Sweden)


#Total buildings within each box buffer

Buildings_in_buffer_Sweden<-st_intersection(Buildings_Sweden_2, buffer_500_Sweden)
mapview(Buildings_in_buffer_Sweden)

Buildings_in_buffer_Sweden$area_m2<-st_area(Buildings_in_buffer_Sweden)
Buildings_in_buffer_Sweden

d_buildings_Sweden <- data.frame(Buildings_in_buffer_Sweden)
head(d_buildings_Sweden)
aggregated_buildings_Sweden<-cast(d_buildings_Sweden, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Sweden_2)

nrow(aggregated_buildings_Sweden)


owls_sf_Sweden_2$ID_CODE
aggregated_buildings_Sweden$ID_CODE

owls_sf_Sweden_2<-left_join(owls_sf_Sweden_2, aggregated_buildings_Sweden, by="ID_CODE")
owls_sf_Sweden_2

owls_sf_Sweden_2[c(40)][is.na(owls_sf_Sweden_2[c(40)])] <- 0


owls_sf_Sweden_2$buildings_prop <- (owls_sf_Sweden_2$building/(500*500*3.14))*100


owls_sf_Sweden_2[c(42)][is.na(owls_sf_Sweden_2[c(42)])] <- 0



################
### SLOVENIA ###
################

Buildings_Slovenia <- st_read("Slovenia_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Slovenia)
st_crs(buffer_500_Slovenia)

Buildings_Slovenia_2<-st_transform(Buildings_Slovenia, st_crs(buffer_500_Slovenia))
st_crs(Buildings_Slovenia_2)

mapview(Buildings_Slovenia_2) 

mapview(owls_sf_Slovenia)


#Total buildings within each box buffer

Buildings_in_buffer_Slovenia<-st_intersection(Buildings_Slovenia_2, buffer_500_Slovenia)
mapview(Buildings_in_buffer_Slovenia)

Buildings_in_buffer_Slovenia$area_m2<-st_area(Buildings_in_buffer_Slovenia)
Buildings_in_buffer_Slovenia

d_buildings_Slovenia <- data.frame(Buildings_in_buffer_Slovenia)
head(d_buildings_Slovenia)
aggregated_buildings_Slovenia<-cast(d_buildings_Slovenia, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Slovenia_2)

nrow(aggregated_buildings_Slovenia)


owls_sf_Slovenia_2$ID_CODE
aggregated_buildings_Slovenia$ID_CODE

owls_sf_Slovenia_2<-left_join(owls_sf_Slovenia_2, aggregated_buildings_Slovenia, by="ID_CODE")
owls_sf_Slovenia_2

owls_sf_Slovenia_2[c(41)][is.na(owls_sf_Slovenia_2[c(41)])] <- 0


owls_sf_Slovenia_2$buildings_prop <- (owls_sf_Slovenia_2$building/(500*500*3.14))*100


owls_sf_Slovenia_2[c(43)][is.na(owls_sf_Slovenia_2[c(43)])] <- 0



###################
### SWITZERLAND ###
###################

Buildings_Switzerland <- st_read("Switzerland_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Switzerland)
st_crs(buffer_500_Switzerland)

Buildings_Switzerland_2<-st_transform(Buildings_Switzerland, st_crs(buffer_500_Switzerland))
st_crs(Buildings_Switzerland_2)

mapview(Buildings_Switzerland_2) 

mapview(owls_sf_Switzerland)


#Total buildings within each box buffer

Buildings_in_buffer_Switzerland<-st_intersection(Buildings_Switzerland_2, buffer_500_Switzerland)
mapview(Buildings_in_buffer_Switzerland)

Buildings_in_buffer_Switzerland$area_m2<-st_area(Buildings_in_buffer_Switzerland)
Buildings_in_buffer_Switzerland

d_buildings_Switzerland <- data.frame(Buildings_in_buffer_Switzerland)
head(d_buildings_Switzerland)
aggregated_buildings_Switzerland<-cast(d_buildings_Switzerland, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Switzerland_2)

nrow(aggregated_buildings_Switzerland)


owls_sf_Switzerland_2$ID_CODE
aggregated_buildings_Switzerland$ID_CODE

owls_sf_Switzerland_2<-left_join(owls_sf_Switzerland_2, aggregated_buildings_Switzerland, by="ID_CODE")
owls_sf_Switzerland_2

owls_sf_Switzerland_2[c(44)][is.na(owls_sf_Switzerland_2[c(44)])] <- 0


owls_sf_Switzerland_2$buildings_prop <- (owls_sf_Switzerland_2$building/(500*500*3.14))*100


owls_sf_Switzerland_2[c(46)][is.na(owls_sf_Switzerland_2[c(46)])] <- 0



###############
### CZECHIA ###
###############

Buildings_Czechia <- st_read("Czechia_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Czechia)
st_crs(buffer_500_Czechia)

Buildings_Czechia_2<-st_transform(Buildings_Czechia, st_crs(buffer_500_Czechia))
st_crs(Buildings_Czechia_2)

mapview(Buildings_Czechia_2) 

mapview(owls_sf_Czechia)


#Total buildings within each box buffer

Buildings_in_buffer_Czechia<-st_intersection(Buildings_Czechia_2, buffer_500_Czechia)
mapview(Buildings_in_buffer_Czechia)

Buildings_in_buffer_Czechia$area_m2<-st_area(Buildings_in_buffer_Czechia)
Buildings_in_buffer_Czechia

d_buildings_Czechia <- data.frame(Buildings_in_buffer_Czechia)
head(d_buildings_Czechia)
aggregated_buildings_Czechia<-cast(d_buildings_Czechia, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Czechia_2)

nrow(aggregated_buildings_Czechia)


owls_sf_Czechia_2$ID_CODE
aggregated_buildings_Czechia$ID_CODE

owls_sf_Czechia_2<-left_join(owls_sf_Czechia_2, aggregated_buildings_Czechia, by="ID_CODE")
owls_sf_Czechia_2

owls_sf_Czechia_2[c(45)][is.na(owls_sf_Czechia_2[c(45)])] <- 0


owls_sf_Czechia_2$buildings_prop <- (owls_sf_Czechia_2$building/(500*500*3.14))*100


owls_sf_Czechia_2[c(47)][is.na(owls_sf_Czechia_2[c(47)])] <- 0



###################
### LITHUANIA a ###
###################

Buildings_Lithuania_a <- st_read("Lithuania_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Lithuania_a)
st_crs(buffer_500_Lithuania_a)

Buildings_Lithuania_a_2<-st_transform(Buildings_Lithuania_a, st_crs(buffer_500_Lithuania_a))
st_crs(Buildings_Lithuania_a_2)

mapview(Buildings_Lithuania_a_2) 

mapview(owls_sf_Lithuania_a)


#Total buildings within each box buffer

Buildings_in_buffer_Lithuania_a<-st_intersection(Buildings_Lithuania_a_2, buffer_500_Lithuania_a)
mapview(Buildings_in_buffer_Lithuania_a)

Buildings_in_buffer_Lithuania_a$area_m2<-st_area(Buildings_in_buffer_Lithuania_a)
Buildings_in_buffer_Lithuania_a

d_buildings_Lithuania_a <- data.frame(Buildings_in_buffer_Lithuania_a)
head(d_buildings_Lithuania_a)
aggregated_buildings_Lithuania_a<-cast(d_buildings_Lithuania_a, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Lithuania_a_2)

nrow(aggregated_buildings_Lithuania_a)


owls_sf_Lithuania_a_2$ID_CODE
aggregated_buildings_Lithuania_a$ID_CODE

owls_sf_Lithuania_a_2<-left_join(owls_sf_Lithuania_a_2, aggregated_buildings_Lithuania_a, by="ID_CODE")
owls_sf_Lithuania_a_2

owls_sf_Lithuania_a_2[c(39)][is.na(owls_sf_Lithuania_a_2[c(39)])] <- 0


owls_sf_Lithuania_a_2$buildings_prop <- (owls_sf_Lithuania_a_2$building/(500*500*3.14))*100


owls_sf_Lithuania_a_2[c(41)][is.na(owls_sf_Lithuania_a_2[c(41)])] <- 0



###################
### LITHUANIA b ###
###################

Buildings_Lithuania_b <- st_read("Lithuania_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Lithuania_b)
st_crs(buffer_500_Lithuania_b)

Buildings_Lithuania_b_2<-st_transform(Buildings_Lithuania_b, st_crs(buffer_500_Lithuania_b))
st_crs(Buildings_Lithuania_b_2)

mapview(Buildings_Lithuania_b_2) 

mapview(owls_sf_Lithuania_b)


#Total buildings within each box buffer

Buildings_in_buffer_Lithuania_b<-st_intersection(Buildings_Lithuania_b_2, buffer_500_Lithuania_b)
mapview(Buildings_in_buffer_Lithuania_b)

Buildings_in_buffer_Lithuania_b$area_m2<-st_area(Buildings_in_buffer_Lithuania_b)
Buildings_in_buffer_Lithuania_b

d_buildings_Lithuania_b <- data.frame(Buildings_in_buffer_Lithuania_b)
head(d_buildings_Lithuania_b)
aggregated_buildings_Lithuania_b<-cast(d_buildings_Lithuania_b, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Lithuania_b_2)

nrow(aggregated_buildings_Lithuania_b)


owls_sf_Lithuania_b_2$ID_CODE
aggregated_buildings_Lithuania_b$ID_CODE

owls_sf_Lithuania_b_2<-left_join(owls_sf_Lithuania_b_2, aggregated_buildings_Lithuania_b, by="ID_CODE")
owls_sf_Lithuania_b_2

owls_sf_Lithuania_b_2[c(41)][is.na(owls_sf_Lithuania_b_2[c(41)])] <- 0


owls_sf_Lithuania_b_2$buildings_prop <- (owls_sf_Lithuania_b_2$building/(500*500*3.14))*100


owls_sf_Lithuania_b_2[c(43)][is.na(owls_sf_Lithuania_b_2[c(43)])] <- 0



################
### NORWAY a ###
################

Buildings_Norway_a <- st_read("Norway_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Norway_a)
st_crs(buffer_500_Norway_a)

Buildings_Norway_a_2<-st_transform(Buildings_Norway_a, st_crs(buffer_500_Norway_a))
st_crs(Buildings_Norway_a_2)

mapview(Buildings_Norway_a_2) 

mapview(owls_sf_Norway_a)


#Total buildings within each box buffer

Buildings_in_buffer_Norway_a<-st_intersection(Buildings_Norway_a_2, buffer_500_Norway_a)
mapview(Buildings_in_buffer_Norway_a)

Buildings_in_buffer_Norway_a$area_m2<-st_area(Buildings_in_buffer_Norway_a)
Buildings_in_buffer_Norway_a

d_buildings_Norway_a <- data.frame(Buildings_in_buffer_Norway_a)
head(d_buildings_Norway_a)
aggregated_buildings_Norway_a<-cast(d_buildings_Norway_a, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Norway_a_2)

nrow(aggregated_buildings_Norway_a)


owls_sf_Norway_a_2$ID_CODE
aggregated_buildings_Norway_a$ID_CODE

owls_sf_Norway_a_2<-left_join(owls_sf_Norway_a_2, aggregated_buildings_Norway_a, by="ID_CODE")
owls_sf_Norway_a_2

owls_sf_Norway_a_2[c(42)][is.na(owls_sf_Norway_a_2[c(42)])] <- 0


owls_sf_Norway_a_2$buildings_prop <- (owls_sf_Norway_a_2$building/(500*500*3.14))*100


owls_sf_Norway_a_2[c(44)][is.na(owls_sf_Norway_a_2[c(44)])] <- 0



################
### NORWAY b ###
################

Buildings_Norway_b <- st_read("Norway_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Norway_b)
st_crs(buffer_500_Norway_b)

Buildings_Norway_b_2<-st_transform(Buildings_Norway_b, st_crs(buffer_500_Norway_b))
st_crs(Buildings_Norway_b_2)

mapview(Buildings_Norway_b_2) 

mapview(owls_sf_Norway_b)


#Total buildings within each box buffer

Buildings_in_buffer_Norway_b<-st_intersection(Buildings_Norway_b_2, buffer_500_Norway_b)
mapview(Buildings_in_buffer_Norway_b)

Buildings_in_buffer_Norway_b$area_m2<-st_area(Buildings_in_buffer_Norway_b)
Buildings_in_buffer_Norway_b

d_buildings_Norway_b <- data.frame(Buildings_in_buffer_Norway_b)
head(d_buildings_Norway_b)
aggregated_buildings_Norway_b<-cast(d_buildings_Norway_b, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Norway_b_2)

nrow(aggregated_buildings_Norway_b)


owls_sf_Norway_b_2$ID_CODE
aggregated_buildings_Norway_b$ID_CODE

owls_sf_Norway_b_2<-left_join(owls_sf_Norway_b_2, aggregated_buildings_Norway_b, by="ID_CODE")
owls_sf_Norway_b_2

owls_sf_Norway_b_2[c(41)][is.na(owls_sf_Norway_b_2[c(41)])] <- 0


owls_sf_Norway_b_2$buildings_prop <- (owls_sf_Norway_b_2$building/(500*500*3.14))*100


owls_sf_Norway_b_2[c(43)][is.na(owls_sf_Norway_b_2[c(43)])] <- 0



#################
### FINLAND a ###
#################

Buildings_Finland_a <- st_read("Finland_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Finland_a)
st_crs(buffer_500_Finland_a)

Buildings_Finland_a_2<-st_transform(Buildings_Finland_a, st_crs(buffer_500_Finland_a))
st_crs(Buildings_Finland_a_2)

mapview(Buildings_Finland_a_2) 

mapview(owls_sf_Finland_a)


#Total buildings within each box buffer

Buildings_in_buffer_Finland_a<-st_intersection(Buildings_Finland_a_2, buffer_500_Finland_a)
mapview(Buildings_in_buffer_Finland_a)

Buildings_in_buffer_Finland_a$area_m2<-st_area(Buildings_in_buffer_Finland_a)
Buildings_in_buffer_Finland_a

d_buildings_Finland_a <- data.frame(Buildings_in_buffer_Finland_a)
head(d_buildings_Finland_a)
aggregated_buildings_Finland_a<-cast(d_buildings_Finland_a, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Finland_a_2)

nrow(aggregated_buildings_Finland_a)


owls_sf_Finland_a_2$ID_CODE
aggregated_buildings_Finland_a$ID_CODE

owls_sf_Finland_a_2<-left_join(owls_sf_Finland_a_2, aggregated_buildings_Finland_a, by="ID_CODE")
owls_sf_Finland_a_2

owls_sf_Finland_a_2[c(43)][is.na(owls_sf_Finland_a_2[c(43)])] <- 0


owls_sf_Finland_a_2$buildings_prop <- (owls_sf_Finland_a_2$building/(500*500*3.14))*100


owls_sf_Finland_a_2[c(45)][is.na(owls_sf_Finland_a_2[c(45)])] <- 0



#################
### FINLAND b ###
#################

Buildings_Finland_b <- st_read("Finland_gis_osm_buildings_a_free_1_crop.shp")

st_crs(Buildings_Finland_b)
st_crs(buffer_500_Finland_b)

Buildings_Finland_b_2<-st_transform(Buildings_Finland_b, st_crs(buffer_500_Finland_b))
st_crs(Buildings_Finland_b_2)

mapview(Buildings_Finland_b_2) 

mapview(owls_sf_Finland_b)


#Total buildings within each box buffer

Buildings_in_buffer_Finland_b<-st_intersection(Buildings_Finland_b_2, buffer_500_Finland_b)
mapview(Buildings_in_buffer_Finland_b)

Buildings_in_buffer_Finland_b$area_m2<-st_area(Buildings_in_buffer_Finland_b)
Buildings_in_buffer_Finland_b

d_buildings_Finland_b <- data.frame(Buildings_in_buffer_Finland_b)
head(d_buildings_Finland_b)
aggregated_buildings_Finland_b<-cast(d_buildings_Finland_b, ID_CODE ~fclass , value="area_m2", fun.aggregate=sum)
head(owls_sf_Finland_b_2)

nrow(aggregated_buildings_Finland_b)


owls_sf_Finland_b_2$ID_CODE
aggregated_buildings_Finland_b$ID_CODE

owls_sf_Finland_b_2<-left_join(owls_sf_Finland_b_2, aggregated_buildings_Finland_b, by="ID_CODE")
owls_sf_Finland_b_2

owls_sf_Finland_b_2[c(42)][is.na(owls_sf_Finland_b_2[c(42)])] <- 0


owls_sf_Finland_b_2$buildings_prop <- (owls_sf_Finland_b_2$building/(500*500*3.14))*100


owls_sf_Finland_b_2[c(44)][is.na(owls_sf_Finland_b_2[c(44)])] <- 0





                                                  #######################################
                                                  #######################################
                                                  #   LIGHT POLLUTION DATA EXTRACTION   #
                                                  #######################################
                                                  #######################################



LP_2012<-rast("viirs_2012_raw.tif")
LP_2013<-rast("viirs_2013_raw.tif")
LP_2014<-rast("viirs_2014_raw.tif")
LP_2015<-rast("viirs_2015_raw.tif")
LP_2016<-rast("viirs_2016_raw.tif")
LP_2017<-rast("viirs_2017_raw.tif")
LP_2018<-rast("viirs_2018_raw.tif")
LP_2019<-rast("viirs_2019_raw.tif")
LP_2020<-rast("viirs_2020_raw.tif")
LP_2021<-rast("viirs_2021_raw.tif")
LP_2022<-rast("viirs_2022_raw.tif")
LP_2023<-rast("viirs_2023_raw.tif")



################
### SCOTLAND ###
################

owls_sf_Scotland_3<-st_transform(owls_sf_Scotland_2, "epsg:4326")

owls_sf_buffer_Scotland<-st_buffer(owls_sf_Scotland_3, dist=500, nQuadSegs = 30)

owls_sf_Scotland_3$LP_mean<-NA
owls_sf_Scotland_3$LP_sd<-NA


owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2011]<-extract(LP_2012, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2011,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2012]<-extract(LP_2012, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2012,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2013]<-extract(LP_2013, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2013,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2014]<-extract(LP_2014, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2014,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2015]<-extract(LP_2015, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2015,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2016]<-extract(LP_2016, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2016,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2017]<-extract(LP_2017, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2017,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2018]<-extract(LP_2018, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2018,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2019]<-extract(LP_2019, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2019,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2020]<-extract(LP_2020, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2020,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2021]<-extract(LP_2021, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2021,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2022]<-extract(LP_2022, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2022,], fun="mean")[,2]
owls_sf_Scotland_3$LP_mean[owls_sf_buffer_Scotland$year==2023]<-extract(LP_2023, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2023,], fun="mean")[,2]


owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2011]<-extract(LP_2012, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2011,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2012]<-extract(LP_2012, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2012,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2013]<-extract(LP_2013, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2013,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2014]<-extract(LP_2014, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2014,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2015]<-extract(LP_2015, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2015,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2016]<-extract(LP_2016, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2016,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2017]<-extract(LP_2017, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2017,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2018]<-extract(LP_2018, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2018,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2019]<-extract(LP_2019, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2019,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2020]<-extract(LP_2020, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2020,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2021]<-extract(LP_2021, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2021,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2022]<-extract(LP_2022, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2022,], fun="sd")[,2]
owls_sf_Scotland_3$LP_sd[owls_sf_buffer_Scotland$year==2023]<-extract(LP_2023, owls_sf_buffer_Scotland[owls_sf_buffer_Scotland$year==2023,], fun="sd")[,2]



##############
### FRANCE ###
##############

owls_sf_France_3<-st_transform(owls_sf_France_2, "epsg:4326")

owls_sf_buffer_France<-st_buffer(owls_sf_France_3, dist=500, nQuadSegs = 30)

owls_sf_France_3$LP_mean<-NA
owls_sf_France_3$LP_sd<-NA


owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2011]<-extract(LP_2012, owls_sf_buffer_France[owls_sf_buffer_France$year==2011,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2012]<-extract(LP_2012, owls_sf_buffer_France[owls_sf_buffer_France$year==2012,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2013]<-extract(LP_2013, owls_sf_buffer_France[owls_sf_buffer_France$year==2013,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2014]<-extract(LP_2014, owls_sf_buffer_France[owls_sf_buffer_France$year==2014,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2015]<-extract(LP_2015, owls_sf_buffer_France[owls_sf_buffer_France$year==2015,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2016]<-extract(LP_2016, owls_sf_buffer_France[owls_sf_buffer_France$year==2016,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2017]<-extract(LP_2017, owls_sf_buffer_France[owls_sf_buffer_France$year==2017,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2018]<-extract(LP_2018, owls_sf_buffer_France[owls_sf_buffer_France$year==2018,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2019]<-extract(LP_2019, owls_sf_buffer_France[owls_sf_buffer_France$year==2019,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2020]<-extract(LP_2020, owls_sf_buffer_France[owls_sf_buffer_France$year==2020,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2021]<-extract(LP_2021, owls_sf_buffer_France[owls_sf_buffer_France$year==2021,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2022]<-extract(LP_2022, owls_sf_buffer_France[owls_sf_buffer_France$year==2022,], fun="mean")[,2]
owls_sf_France_3$LP_mean[owls_sf_buffer_France$year==2023]<-extract(LP_2023, owls_sf_buffer_France[owls_sf_buffer_France$year==2023,], fun="mean")[,2]


owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2011]<-extract(LP_2012, owls_sf_buffer_France[owls_sf_buffer_France$year==2011,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2012]<-extract(LP_2012, owls_sf_buffer_France[owls_sf_buffer_France$year==2012,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2013]<-extract(LP_2013, owls_sf_buffer_France[owls_sf_buffer_France$year==2013,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2014]<-extract(LP_2014, owls_sf_buffer_France[owls_sf_buffer_France$year==2014,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2015]<-extract(LP_2015, owls_sf_buffer_France[owls_sf_buffer_France$year==2015,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2016]<-extract(LP_2016, owls_sf_buffer_France[owls_sf_buffer_France$year==2016,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2017]<-extract(LP_2017, owls_sf_buffer_France[owls_sf_buffer_France$year==2017,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2018]<-extract(LP_2018, owls_sf_buffer_France[owls_sf_buffer_France$year==2018,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2019]<-extract(LP_2019, owls_sf_buffer_France[owls_sf_buffer_France$year==2019,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2020]<-extract(LP_2020, owls_sf_buffer_France[owls_sf_buffer_France$year==2020,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2021]<-extract(LP_2021, owls_sf_buffer_France[owls_sf_buffer_France$year==2021,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2022]<-extract(LP_2022, owls_sf_buffer_France[owls_sf_buffer_France$year==2022,], fun="sd")[,2]
owls_sf_France_3$LP_sd[owls_sf_buffer_France$year==2023]<-extract(LP_2023, owls_sf_buffer_France[owls_sf_buffer_France$year==2023,], fun="sd")[,2]



##############
### SWEDEN ###
##############

# N.B. for Sweden no breeding and occupancy data for 2018 and 2023

owls_sf_Sweden_3<-st_transform(owls_sf_Sweden_2, "epsg:4326")

owls_sf_buffer_Sweden<-st_buffer(owls_sf_Sweden_3, dist=500, nQuadSegs = 30)

owls_sf_Sweden_3$LP_mean<-NA
owls_sf_Sweden_3$LP_sd<-NA


owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2011]<-extract(LP_2012, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2011,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2012]<-extract(LP_2012, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2012,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2013]<-extract(LP_2013, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2013,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2014]<-extract(LP_2014, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2014,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2015]<-extract(LP_2015, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2015,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2016]<-extract(LP_2016, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2016,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2017]<-extract(LP_2017, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2017,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2018]<-extract(LP_2018, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2018,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2019]<-extract(LP_2019, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2019,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2020]<-extract(LP_2020, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2020,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2021]<-extract(LP_2021, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2021,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2022]<-extract(LP_2022, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2022,], fun="mean")[,2]
owls_sf_Sweden_3$LP_mean[owls_sf_buffer_Sweden$year==2023]<-extract(LP_2023, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2023,], fun="mean")[,2]


owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2011]<-extract(LP_2012, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2011,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2012]<-extract(LP_2012, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2012,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2013]<-extract(LP_2013, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2013,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2014]<-extract(LP_2014, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2014,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2015]<-extract(LP_2015, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2015,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2016]<-extract(LP_2016, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2016,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2017]<-extract(LP_2017, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2017,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2018]<-extract(LP_2018, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2018,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2019]<-extract(LP_2019, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2019,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2020]<-extract(LP_2020, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2020,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2021]<-extract(LP_2021, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2021,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2022]<-extract(LP_2022, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2022,], fun="sd")[,2]
owls_sf_Sweden_3$LP_sd[owls_sf_buffer_Sweden$year==2023]<-extract(LP_2023, owls_sf_buffer_Sweden[owls_sf_buffer_Sweden$year==2023,], fun="sd")[,2]



################
### SLOVENIA ###
################

owls_sf_Slovenia_3<-st_transform(owls_sf_Slovenia_2, "epsg:4326")

owls_sf_buffer_Slovenia<-st_buffer(owls_sf_Slovenia_3, dist=500, nQuadSegs = 30)

owls_sf_Slovenia_3$LP_mean<-NA
owls_sf_Slovenia_3$LP_sd<-NA


owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2011]<-extract(LP_2012, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2011,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2012]<-extract(LP_2012, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2012,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2013]<-extract(LP_2013, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2013,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2014]<-extract(LP_2014, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2014,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2015]<-extract(LP_2015, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2015,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2016]<-extract(LP_2016, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2016,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2017]<-extract(LP_2017, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2017,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2018]<-extract(LP_2018, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2018,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2019]<-extract(LP_2019, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2019,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2020]<-extract(LP_2020, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2020,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2021]<-extract(LP_2021, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2021,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2022]<-extract(LP_2022, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2022,], fun="mean")[,2]
owls_sf_Slovenia_3$LP_mean[owls_sf_buffer_Slovenia$year==2023]<-extract(LP_2023, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2023,], fun="mean")[,2]


owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2011]<-extract(LP_2012, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2011,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2012]<-extract(LP_2012, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2012,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2013]<-extract(LP_2013, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2013,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2014]<-extract(LP_2014, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2014,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2015]<-extract(LP_2015, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2015,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2016]<-extract(LP_2016, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2016,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2017]<-extract(LP_2017, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2017,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2018]<-extract(LP_2018, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2018,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2019]<-extract(LP_2019, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2019,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2020]<-extract(LP_2020, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2020,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2021]<-extract(LP_2021, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2021,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2022]<-extract(LP_2022, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2022,], fun="sd")[,2]
owls_sf_Slovenia_3$LP_sd[owls_sf_buffer_Slovenia$year==2023]<-extract(LP_2023, owls_sf_buffer_Slovenia[owls_sf_buffer_Slovenia$year==2023,], fun="sd")[,2]



###################
### SWITZERLAND ###
###################

owls_sf_Switzerland_3<-st_transform(owls_sf_Switzerland_2, "epsg:4326")

owls_sf_buffer_Switzerland<-st_buffer(owls_sf_Switzerland_3, dist=500, nQuadSegs = 30)

owls_sf_Switzerland_3$LP_mean<-NA
owls_sf_Switzerland_3$LP_sd<-NA


owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2011]<-extract(LP_2012, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2011,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2012]<-extract(LP_2012, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2012,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2013]<-extract(LP_2013, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2013,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2014]<-extract(LP_2014, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2014,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2015]<-extract(LP_2015, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2015,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2016]<-extract(LP_2016, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2016,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2017]<-extract(LP_2017, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2017,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2018]<-extract(LP_2018, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2018,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2019]<-extract(LP_2019, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2019,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2020]<-extract(LP_2020, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2020,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2021]<-extract(LP_2021, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2021,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2022]<-extract(LP_2022, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2022,], fun="mean")[,2]
owls_sf_Switzerland_3$LP_mean[owls_sf_buffer_Switzerland$year==2023]<-extract(LP_2023, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2023,], fun="mean")[,2]


owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2011]<-extract(LP_2012, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2011,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2012]<-extract(LP_2012, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2012,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2013]<-extract(LP_2013, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2013,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2014]<-extract(LP_2014, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2014,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2015]<-extract(LP_2015, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2015,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2016]<-extract(LP_2016, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2016,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2017]<-extract(LP_2017, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2017,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2018]<-extract(LP_2018, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2018,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2019]<-extract(LP_2019, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2019,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2020]<-extract(LP_2020, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2020,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2021]<-extract(LP_2021, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2021,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2022]<-extract(LP_2022, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2022,], fun="sd")[,2]
owls_sf_Switzerland_3$LP_sd[owls_sf_buffer_Switzerland$year==2023]<-extract(LP_2023, owls_sf_buffer_Switzerland[owls_sf_buffer_Switzerland$year==2023,], fun="sd")[,2]



###############
### CZECHIA ###
###############

owls_sf_Czechia_3<-st_transform(owls_sf_Czechia_2, "epsg:4326")

owls_sf_buffer_Czechia<-st_buffer(owls_sf_Czechia_3, dist=500, nQuadSegs = 30)

owls_sf_Czechia_3$LP_mean<-NA
owls_sf_Czechia_3$LP_sd<-NA


owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2011]<-extract(LP_2012, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2011,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2012]<-extract(LP_2012, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2012,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2013]<-extract(LP_2013, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2013,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2014]<-extract(LP_2014, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2014,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2015]<-extract(LP_2015, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2015,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2016]<-extract(LP_2016, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2016,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2017]<-extract(LP_2017, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2017,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2018]<-extract(LP_2018, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2018,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2019]<-extract(LP_2019, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2019,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2020]<-extract(LP_2020, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2020,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2021]<-extract(LP_2021, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2021,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2022]<-extract(LP_2022, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2022,], fun="mean")[,2]
owls_sf_Czechia_3$LP_mean[owls_sf_buffer_Czechia$year==2023]<-extract(LP_2023, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2023,], fun="mean")[,2]


owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2011]<-extract(LP_2012, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2011,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2012]<-extract(LP_2012, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2012,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2013]<-extract(LP_2013, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2013,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2014]<-extract(LP_2014, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2014,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2015]<-extract(LP_2015, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2015,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2016]<-extract(LP_2016, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2016,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2017]<-extract(LP_2017, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2017,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2018]<-extract(LP_2018, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2018,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2019]<-extract(LP_2019, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2019,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2020]<-extract(LP_2020, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2020,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2021]<-extract(LP_2021, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2021,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2022]<-extract(LP_2022, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2022,], fun="sd")[,2]
owls_sf_Czechia_3$LP_sd[owls_sf_buffer_Czechia$year==2023]<-extract(LP_2023, owls_sf_buffer_Czechia[owls_sf_buffer_Czechia$year==2023,], fun="sd")[,2]



###################
### LITHUANIA a ###
###################

owls_sf_Lithuania_a_3<-st_transform(owls_sf_Lithuania_a_2, "epsg:4326")

owls_sf_buffer_Lithuania_a<-st_buffer(owls_sf_Lithuania_a_3, dist=500, nQuadSegs = 30)

owls_sf_Lithuania_a_3$LP_mean<-NA
owls_sf_Lithuania_a_3$LP_sd<-NA


owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2011]<-extract(LP_2012, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2011,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2012]<-extract(LP_2012, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2012,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2013]<-extract(LP_2013, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2013,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2014]<-extract(LP_2014, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2014,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2015]<-extract(LP_2015, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2015,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2016]<-extract(LP_2016, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2016,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2017]<-extract(LP_2017, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2017,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2018]<-extract(LP_2018, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2018,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2019]<-extract(LP_2019, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2019,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2020]<-extract(LP_2020, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2020,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2021]<-extract(LP_2021, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2021,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2022]<-extract(LP_2022, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2022,], fun="mean")[,2]
owls_sf_Lithuania_a_3$LP_mean[owls_sf_buffer_Lithuania_a$year==2023]<-extract(LP_2023, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2023,], fun="mean")[,2]


owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2011]<-extract(LP_2012, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2011,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2012]<-extract(LP_2012, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2012,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2013]<-extract(LP_2013, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2013,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2014]<-extract(LP_2014, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2014,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2015]<-extract(LP_2015, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2015,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2016]<-extract(LP_2016, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2016,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2017]<-extract(LP_2017, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2017,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2018]<-extract(LP_2018, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2018,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2019]<-extract(LP_2019, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2019,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2020]<-extract(LP_2020, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2020,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2021]<-extract(LP_2021, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2021,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2022]<-extract(LP_2022, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2022,], fun="sd")[,2]
owls_sf_Lithuania_a_3$LP_sd[owls_sf_buffer_Lithuania_a$year==2023]<-extract(LP_2023, owls_sf_buffer_Lithuania_a[owls_sf_buffer_Lithuania_a$year==2023,], fun="sd")[,2]



###################
### LITHUANIA b ###
###################

owls_sf_Lithuania_b_3<-st_transform(owls_sf_Lithuania_b_2, "epsg:4326")

owls_sf_buffer_Lithuania_b<-st_buffer(owls_sf_Lithuania_b_3, dist=500, nQuadSegs = 30)

owls_sf_Lithuania_b_3$LP_mean<-NA
owls_sf_Lithuania_b_3$LP_sd<-NA


owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2011]<-extract(LP_2012, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2011,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2012]<-extract(LP_2012, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2012,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2013]<-extract(LP_2013, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2013,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2014]<-extract(LP_2014, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2014,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2015]<-extract(LP_2015, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2015,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2016]<-extract(LP_2016, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2016,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2017]<-extract(LP_2017, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2017,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2018]<-extract(LP_2018, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2018,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2019]<-extract(LP_2019, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2019,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2020]<-extract(LP_2020, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2020,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2021]<-extract(LP_2021, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2021,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2022]<-extract(LP_2022, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2022,], fun="mean")[,2]
owls_sf_Lithuania_b_3$LP_mean[owls_sf_buffer_Lithuania_b$year==2023]<-extract(LP_2023, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2023,], fun="mean")[,2]


owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2011]<-extract(LP_2012, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2011,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2012]<-extract(LP_2012, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2012,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2013]<-extract(LP_2013, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2013,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2014]<-extract(LP_2014, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2014,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2015]<-extract(LP_2015, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2015,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2016]<-extract(LP_2016, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2016,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2017]<-extract(LP_2017, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2017,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2018]<-extract(LP_2018, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2018,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2019]<-extract(LP_2019, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2019,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2020]<-extract(LP_2020, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2020,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2021]<-extract(LP_2021, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2021,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2022]<-extract(LP_2022, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2022,], fun="sd")[,2]
owls_sf_Lithuania_b_3$LP_sd[owls_sf_buffer_Lithuania_b$year==2023]<-extract(LP_2023, owls_sf_buffer_Lithuania_b[owls_sf_buffer_Lithuania_b$year==2023,], fun="sd")[,2]



################
### NORWAY a ###
################

owls_sf_Norway_a_3<-st_transform(owls_sf_Norway_a_2, "epsg:4326")

owls_sf_buffer_Norway_a<-st_buffer(owls_sf_Norway_a_3, dist=500, nQuadSegs = 30)

owls_sf_Norway_a_3$LP_mean<-NA
owls_sf_Norway_a_3$LP_sd<-NA


owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2011]<-extract(LP_2012, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2011,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2012]<-extract(LP_2012, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2012,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2013]<-extract(LP_2013, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2013,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2014]<-extract(LP_2014, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2014,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2015]<-extract(LP_2015, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2015,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2016]<-extract(LP_2016, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2016,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2017]<-extract(LP_2017, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2017,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2018]<-extract(LP_2018, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2018,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2019]<-extract(LP_2019, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2019,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2020]<-extract(LP_2020, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2020,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2021]<-extract(LP_2021, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2021,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2022]<-extract(LP_2022, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2022,], fun="mean")[,2]
owls_sf_Norway_a_3$LP_mean[owls_sf_buffer_Norway_a$year==2023]<-extract(LP_2023, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2023,], fun="mean")[,2]


owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2011]<-extract(LP_2012, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2011,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2012]<-extract(LP_2012, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2012,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2013]<-extract(LP_2013, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2013,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2014]<-extract(LP_2014, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2014,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2015]<-extract(LP_2015, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2015,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2016]<-extract(LP_2016, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2016,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2017]<-extract(LP_2017, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2017,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2018]<-extract(LP_2018, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2018,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2019]<-extract(LP_2019, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2019,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2020]<-extract(LP_2020, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2020,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2021]<-extract(LP_2021, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2021,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2022]<-extract(LP_2022, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2022,], fun="sd")[,2]
owls_sf_Norway_a_3$LP_sd[owls_sf_buffer_Norway_a$year==2023]<-extract(LP_2023, owls_sf_buffer_Norway_a[owls_sf_buffer_Norway_a$year==2023,], fun="sd")[,2]



################
### NORWAY b ###
################

# N.B. In Norway b only some years are present

owls_sf_Norway_b_3<-st_transform(owls_sf_Norway_b_2, "epsg:4326")

owls_sf_buffer_Norway_b<-st_buffer(owls_sf_Norway_b_3, dist=500, nQuadSegs = 30)

owls_sf_Norway_b_3$LP_mean<-NA
owls_sf_Norway_b_3$LP_sd<-NA


owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2011]<-extract(LP_2012, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2011,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2012]<-extract(LP_2012, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2012,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2013]<-extract(LP_2013, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2013,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2014]<-extract(LP_2014, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2014,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2015]<-extract(LP_2015, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2015,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2016]<-extract(LP_2016, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2016,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2017]<-extract(LP_2017, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2017,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2018]<-extract(LP_2018, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2018,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2019]<-extract(LP_2019, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2019,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2020]<-extract(LP_2020, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2020,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2021]<-extract(LP_2021, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2021,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2022]<-extract(LP_2022, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2022,], fun="mean")[,2]
owls_sf_Norway_b_3$LP_mean[owls_sf_buffer_Norway_b$year==2023]<-extract(LP_2023, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2023,], fun="mean")[,2]


owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2011]<-extract(LP_2012, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2011,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2012]<-extract(LP_2012, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2012,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2013]<-extract(LP_2013, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2013,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2014]<-extract(LP_2014, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2014,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2015]<-extract(LP_2015, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2015,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2016]<-extract(LP_2016, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2016,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2017]<-extract(LP_2017, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2017,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2018]<-extract(LP_2018, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2018,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2019]<-extract(LP_2019, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2019,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2020]<-extract(LP_2020, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2020,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2021]<-extract(LP_2021, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2021,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2022]<-extract(LP_2022, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2022,], fun="sd")[,2]
owls_sf_Norway_b_3$LP_sd[owls_sf_buffer_Norway_b$year==2023]<-extract(LP_2023, owls_sf_buffer_Norway_b[owls_sf_buffer_Norway_b$year==2023,], fun="sd")[,2]



#################
### FINLAND a ###
#################

owls_sf_Finland_a_3<-st_transform(owls_sf_Finland_a_2, "epsg:4326")

owls_sf_buffer_Finland_a<-st_buffer(owls_sf_Finland_a_3, dist=500, nQuadSegs = 30)

owls_sf_Finland_a_3$LP_mean<-NA
owls_sf_Finland_a_3$LP_sd<-NA


owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2011]<-extract(LP_2012, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2011,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2012]<-extract(LP_2012, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2012,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2013]<-extract(LP_2013, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2013,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2014]<-extract(LP_2014, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2014,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2015]<-extract(LP_2015, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2015,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2016]<-extract(LP_2016, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2016,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2017]<-extract(LP_2017, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2017,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2018]<-extract(LP_2018, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2018,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2019]<-extract(LP_2019, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2019,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2020]<-extract(LP_2020, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2020,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2021]<-extract(LP_2021, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2021,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2022]<-extract(LP_2022, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2022,], fun="mean")[,2]
owls_sf_Finland_a_3$LP_mean[owls_sf_buffer_Finland_a$year==2023]<-extract(LP_2023, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2023,], fun="mean")[,2]


owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2011]<-extract(LP_2012, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2011,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2012]<-extract(LP_2012, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2012,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2013]<-extract(LP_2013, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2013,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2014]<-extract(LP_2014, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2014,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2015]<-extract(LP_2015, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2015,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2016]<-extract(LP_2016, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2016,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2017]<-extract(LP_2017, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2017,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2018]<-extract(LP_2018, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2018,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2019]<-extract(LP_2019, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2019,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2020]<-extract(LP_2020, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2020,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2021]<-extract(LP_2021, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2021,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2022]<-extract(LP_2022, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2022,], fun="sd")[,2]
owls_sf_Finland_a_3$LP_sd[owls_sf_buffer_Finland_a$year==2023]<-extract(LP_2023, owls_sf_buffer_Finland_a[owls_sf_buffer_Finland_a$year==2023,], fun="sd")[,2]



#################
### FINLAND b ###
#################

# N.B. In Finland b only some years are present

owls_sf_Finland_b_3<-st_transform(owls_sf_Finland_b_2, "epsg:4326")

owls_sf_buffer_Finland_b<-st_buffer(owls_sf_Finland_b_3, dist=500, nQuadSegs = 30)

owls_sf_Finland_b_3$LP_mean<-NA
owls_sf_Finland_b_3$LP_sd<-NA


owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2011]<-extract(LP_2012, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2011,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2012]<-extract(LP_2012, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2012,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2013]<-extract(LP_2013, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2013,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2014]<-extract(LP_2014, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2014,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2015]<-extract(LP_2015, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2015,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2016]<-extract(LP_2016, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2016,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2017]<-extract(LP_2017, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2017,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2018]<-extract(LP_2018, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2018,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2019]<-extract(LP_2019, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2019,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2020]<-extract(LP_2020, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2020,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2021]<-extract(LP_2021, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2021,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2022]<-extract(LP_2022, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2022,], fun="mean")[,2]
owls_sf_Finland_b_3$LP_mean[owls_sf_buffer_Finland_b$year==2023]<-extract(LP_2023, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2023,], fun="mean")[,2]


owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2011]<-extract(LP_2012, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2011,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2012]<-extract(LP_2012, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2012,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2013]<-extract(LP_2013, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2013,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2014]<-extract(LP_2014, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2014,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2015]<-extract(LP_2015, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2015,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2016]<-extract(LP_2016, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2016,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2017]<-extract(LP_2017, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2017,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2018]<-extract(LP_2018, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2018,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2019]<-extract(LP_2019, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2019,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2020]<-extract(LP_2020, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2020,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2021]<-extract(LP_2021, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2021,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2022]<-extract(LP_2022, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2022,], fun="sd")[,2]
owls_sf_Finland_b_3$LP_sd[owls_sf_buffer_Finland_b$year==2023]<-extract(LP_2023, owls_sf_buffer_Finland_b[owls_sf_buffer_Finland_b$year==2023,], fun="sd")[,2]





######################################################
        #   REMOVE COLUMNS OF ROADS NOT NEEDED #
######################################################


owls_sf_Scotland_4 <- owls_sf_Scotland_3 %>% select(-motorway, -motorway_link, -trunk, -primary, -primary_link, -secondary,
                                                    -tertiary, -tertiary_link, -residential)


owls_sf_France_4 <- owls_sf_France_3 %>% select(-motorway, -trunk, -trunk_link, -primary, -secondary,
                                                    -tertiary, -residential)


owls_sf_Sweden_4 <- owls_sf_Sweden_3 %>% select(-primary, -secondary,-tertiary, -residential)


owls_sf_Slovenia_4 <- owls_sf_Slovenia_3 %>% select(-primary, -primary_link, -secondary, -tertiary, -residential)


owls_sf_Switzerland_4 <- owls_sf_Switzerland_3 %>% select(-motorway, -motorway_link, -trunk, -trunk_link, -primary, -secondary,
                                                    -tertiary, -residential)


owls_sf_Czechia_4 <- owls_sf_Czechia_3 %>% select(-motorway, -motorway_link, -trunk, -trunk_link, -primary, -primary_link, 
                                                  -secondary, -tertiary, -residential)


owls_sf_Lithuania_a_4 <- owls_sf_Lithuania_a_3 %>% select(-secondary, -tertiary, -residential)


owls_sf_Lithuania_b_4 <- owls_sf_Lithuania_b_3 %>% select(-motorway, -motorway_link, -secondary, -tertiary, -residential)


owls_sf_Norway_a_4 <- owls_sf_Norway_a_3 %>% select(-trunk, -trunk_link, -primary, -secondary, -tertiary, -residential)


owls_sf_Norway_b_4 <- owls_sf_Norway_b_3 %>% select(-trunk, -primary, -secondary, -tertiary, -residential)


owls_sf_Finland_a_4 <- owls_sf_Finland_a_3 %>% select(-motorway, -motorway_link, -trunk, -secondary, -tertiary, -tertiary_link,
                                                      -residential)


owls_sf_Finland_b_4 <- owls_sf_Finland_b_3 %>% select(-trunk, -trunk_link, -primary, -secondary, -tertiary, -residential)




################################################
        #   SEPARATE COORDS IN 2 COLUMNS #
################################################


library(purrr)

owls_Scotland_final <- owls_sf_Scotland_4 %>% 
  mutate(long = unlist(map(owls_sf_Scotland_4$geometry,1)),
        lat = unlist(map(owls_sf_Scotland_4$geometry,2)))


owls_France_final <- owls_sf_France_4 %>% 
  mutate(long = unlist(map(owls_sf_France_4$geometry,1)),
         lat = unlist(map(owls_sf_France_4$geometry,2)))


owls_Sweden_final <- owls_sf_Sweden_4 %>% 
  mutate(long = unlist(map(owls_sf_Sweden_4$geometry,1)),
         lat = unlist(map(owls_sf_Sweden_4$geometry,2)))


owls_Slovenia_final <- owls_sf_Slovenia_4 %>% 
  mutate(long = unlist(map(owls_sf_Slovenia_4$geometry,1)),
         lat = unlist(map(owls_sf_Slovenia_4$geometry,2)))


owls_Switzerland_final <- owls_sf_Switzerland_4 %>% 
  mutate(long = unlist(map(owls_sf_Switzerland_4$geometry,1)),
         lat = unlist(map(owls_sf_Switzerland_4$geometry,2)))


owls_Czechia_final <- owls_sf_Czechia_4 %>% 
  mutate(long = unlist(map(owls_sf_Czechia_4$geometry,1)),
         lat = unlist(map(owls_sf_Czechia_4$geometry,2)))


owls_Lithuania_a_final <- owls_sf_Lithuania_a_4 %>% 
  mutate(long = unlist(map(owls_sf_Lithuania_a_4$geometry,1)),
         lat = unlist(map(owls_sf_Lithuania_a_4$geometry,2)))


owls_Lithuania_b_final <- owls_sf_Lithuania_b_4 %>% 
  mutate(long = unlist(map(owls_sf_Lithuania_b_4$geometry,1)),
         lat = unlist(map(owls_sf_Lithuania_b_4$geometry,2)))


owls_Norway_a_final <- owls_sf_Norway_a_4 %>% 
  mutate(long = unlist(map(owls_sf_Norway_a_4$geometry,1)),
         lat = unlist(map(owls_sf_Norway_a_4$geometry,2)))


owls_Norway_b_final <- owls_sf_Norway_b_4 %>% 
  mutate(long = unlist(map(owls_sf_Norway_b_4$geometry,1)),
         lat = unlist(map(owls_sf_Norway_b_4$geometry,2)))


owls_Finland_a_final <- owls_sf_Finland_a_4 %>% 
  mutate(long = unlist(map(owls_sf_Finland_a_4$geometry,1)),
         lat = unlist(map(owls_sf_Finland_a_4$geometry,2)))


owls_Finland_b_final <- owls_sf_Finland_b_4 %>% 
  mutate(long = unlist(map(owls_sf_Finland_b_4$geometry,1)),
         lat = unlist(map(owls_sf_Finland_b_4$geometry,2)))




######################################
        #   MERGE DATASETS #
######################################


Tawny_Owl_Europe <- rbind(owls_Scotland_final, owls_France_final, owls_Sweden_final, owls_Slovenia_final, owls_Switzerland_final,
                          owls_Czechia_final, owls_Lithuania_a_final, owls_Lithuania_b_final, owls_Norway_a_final, owls_Norway_b_final,
                          owls_Finland_a_final, owls_Finland_b_final)




######################################
        #   EXPORT DATASET #
######################################


write.csv(Tawny_Owl_Europe, file = "Tawny Owl Europe.csv")






###################  extra code I used to check details before merging datasets  #####################  

#to check which columns' names did not match
names(owls_France_final)==names(owls_Sweden_final)

#rename the column with the correct name
colnames(owls_Sweden_final)[which(names(owls_Sweden_final) == "noise_mea")] <- "noise_mean"

names(owls_Switzerland_final)==names(owls_Czechia_final)

colnames(owls_Czechia_final)[which(names(owls_Czechia_final) == "occupancy_other_occupant ")] <- "occupancy_other_occupant"

names(owls_Czechia_final)==names(owls_Lithuania_a_final)

colnames(owls_Lithuania_a_final)[which(names(owls_Lithuania_a_final) == "occupancy_other_occupant
")] <- "occupancy_other_occupant"

#####################




