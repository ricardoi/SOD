#----- loading packages

library(raster)
library(rgdal)
library(sf)
library(stars)
library(maptools)
library(mapdata)

data(wrld_simpl) #add country boundary to the map

# setting working directory
setwd("~/Documents/Maps/Tree_species/")
forestden <- raster("rasters/lide3_ba_2017.tif")  
forestden <- read_stars("rasters/lide3_ba_2017.tif")  

fd <- st_as_sf(forestden)
fd
st_crs(fd)
crs(fd)


# transforming proj = aea (Alberts equal area)
fd <- st_set_crs(fd, 26919)# 26919 code is equivalent to UTM NAD83 Zone 19N EPSG

fd.gcs <- st_transform(forestden, "+proj=longlat +datum=WGS84")
st_crs(fd.gcs)

# ploting map
plot(forestden)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')

# Exploring raster
st_crs(forestden)
crs(forestden)

load(url("https://github.com/mgimond/Spatial/raw/main/Data/Sample1.RData"))

class(s.sf)
