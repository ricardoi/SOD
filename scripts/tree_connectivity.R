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
tree <- 'tanoek'                                   ###

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
latito    <- -123.7                                  ###
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
cropharvestRasterAgg <- aggregate(fd, fact = resolutionscale, fun=sum, na.action = na.omit) #aggregate raster as specified resolution, function=sum
cropharvestRasterAgg <- cropharvestRasterAgg /resolutionscale /resolutionscale
totalmean <- cropharvestRasterAgg
structure(cropharvestRasterAgg)

# Set pallette for map
palette = colPallett <- c("#F6E726FF","#FEBC2AFF","#F9963FFF", "#EB7557FF", "#D7566CFF","#BE3885FF", "#B32C8EFF","#AD2793FF", "#A11A9CFF", "#8B0AA5FF", "#7C02A8FF", "#7401A8FF", "#6C00A8FF", "#6400A7FF", "#5C01A6FF", "#5C01A6FF", "#5402A3FF","#5402A3FF", "#4B03A1FF", "#43039EFF", "#3A049AFF", "#3A049AFF", "#300597FF", "#300597FF", "#270591FF","#270591FF",  "#1B068DFF","#1B068DFF", "#1B068DFF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF","#0D0887FF")

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland
zrAggTmean <- range( 0.000000000001, max(getValues(totalmean), na.rm = TRUE))
plot(cropharvestRasterAgg, main=paste('Harvested area fraction based on "total mean" :', 
                                       tree, CellDegree, 'degree'), col=palette, zlim= zrAggTmean,  
                                      xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland for Caribbean area
plot(cropharvestRasterWagg, main=paste('Harvested area fraction based on "total mean" :', crop, CellDegree, 'degree Caribbean area'), col=palette2, zlim= zrAggTmean,  xaxt='n',  yaxt='n', axes=F, box=F, ylim=c(latifrom,latito), xlim=c(longifrom,longito))
plot(countriesLow, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')
