#----- loading packages
library(raster)
library(rgdal)
library(sf)
library(stars)
library(maptools)
library(mapdata)

data(wrld_simpl) #add country boundary to the map

#----- setting working directory
setwd("~/Documents/Maps/Tree_species/")


#----- loading data
forestden <- raster("rasters/lide3_ba_2017.tif")  
# forestden <- read_stars("rasters/lide3_ba_2017.tif")  

fd <- projectRaster(forestden, 
      crs ="+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
fd
st_crs(fd)
crs(fd)

# transforming proj = aea (Azimutal equal area)

# ploting map
plot(fd)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')

# Exploring raster
st_crs(fd)
crs(fd)
