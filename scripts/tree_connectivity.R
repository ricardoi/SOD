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
#-------------------------------------
# Set map resolution
resolutionscale <- 6  # 6 of 5-min cells for 0.5 degree ###
CellDegree      <- 0.5       

#----- loading data
forestden <- raster("rasters/lide3_ba_2017.tif")  
# forestden <- read_stars("rasters/lide3_ba_2017.tif")  

fd <- projectRaster(forestden, 
      crs ="+proj=longlat +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") #utm
fd
st_crs(fd)
crs(fd)

# transforming proj = aea (Azimutal equal area)
# Range for Southwestern Oregon area               ###
latifrom  <- -124.5 #latitude: from -48 to 76      ###
latito    <- -124                                  ###
longifrom <- 42 #longitude: from -24 to 180       ###
longito   <- 42.5                                 ###

# ploting map
plot(fd)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')
#-----------------------------------------------------
plot(fd, xlim=c(latifrom,latito), ylim=c(longifrom,longito))
# plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')
#-----------------------------------------------------


# Aggregate raster total mean
cropharvestRasterAgg <- aggregate(fd, fact = resolutionscale, fun=sum, na.action = na.omit ) #aggregate raster as specified resolution, function=sum
cropharvestRasterAgg <- cropharvestRasterAgg /resolutionscale /resolutionscale
totalmean <- cropharvestRasterAgg
structure(cropharvestRasterAgg)
