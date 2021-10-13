#----- loading packages

library(raster)
library(rgdal)

library(maptools)
library(mapdata)

data(wrld_simpl) #add country boundary to the map

# setting working directory
setwd("~/Documents/Maps/")
forestden <- raster("Tree_species/rasters/lide3_ba_2017.tif")  



# ploting map
plot(forestden)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')

