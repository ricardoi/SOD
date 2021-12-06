#----- loading packages
library(raster)
library(rgdal)
library(sf)
library(stars)
library(maptools)
library(mapdata)
library(rworldmap)
library(rworldxtra)


data(wrld_simpl) #add country boundary to the map

#----- setting working directory
setwd("~/Documents/Maps/Tree_species/")
source("~/git_local/SOD/scripts/cropland_connectivity_functions.R")
#-----------------------------------------------------
# Set map resolution
resolutionscale <- 6  # 6 of 5-min cells for 0.5 degree ###
CellDegree <- 0.5                                  ###       
tree <- 'tanoek'                                   ###
#-----------------------------------------------------
# Customize crop and values of parameters
beta0 <- 0.5                                       ###
beta <- 1                                          ###
beta1 <- 1.5                                       ###
gamma00 <- 0.05                                    ###
gamma0 <- 0.1                                      ###
gamma <- 0.2                                       ###
gamma1 <- 0.3                                      ###
gamma2 <- 1                                        ###
crop <- 'forest'                                   ###
pcutoff0 <- 0.0015  #position cutoff               ###
pcutoff <- 0.002  #position cutoff                 ###
pcutoff1 <- 0.0025  #position cutoff               ###
cutoffadja0 <- 0.001 # cutoff of adjancecy matrix  ###
cutoffadja <- 0.0001 # cutoff of adjancecy matrix  ###
cutoffadja1 <- 0.00001 # cutoff of adjancecy matrix###
#-----------------------------------------------------
# transforming proj = aea (Azimutal equal area)
# Range for Southwestern Oregon area               ###
latifrom  <- -124.5 #latitude: from -48 to 76      ###
latito    <- -123.7                                ###
longifrom <- 42 #longitude: from -24 to 180        ###
longito   <- 42.5                                  ###
#-----------------------------------------------------
#----- loading data
forestden <- raster("rasters/lide3_ba_2017.tif")  
fd <- projectRaster(forestden, 
      crs ="+proj=longlat +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") #utm
fd
st_crs(fd)
crs(fd)

ncell(fd)
hasValues(fd)
#values(fd) <- 1:ncell(fd)
values(fd)[1:10]

# ploting map
plot(fd)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')
#-----------------------------------------------------
plot(fd, xlim=c(latifrom,latito), ylim=c(longifrom,longito))
# plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')
#-----------------------------------------------------
# crop area
## Crop to the desired extent, then plot
fd.c <- crop(fd, extent(latifrom, latito, longifrom, longito))
wrld_simpl <- crop(wrld_simpl, extent(latifrom, latito, longifrom, longito))
plot(fd.c)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#-----------------------------------------------------
# rescale
res(fd.c)
dim(fd.c)
cellStats(fd.c, 'sum')

## Note: In addition, non-forest pixels in the rasters are designated by a "-1" value - 
## GNN is only applicable in forested areas and non-forest areas were not modeled.
# Changing -1 to NA
values(fd.c)
library(tidyverse)
values(fd.c) <- na_if(values(fd.c), -1)

# Normalizing the data values
fd.max <- max(na.omit((values(fd.c))))
values(fd.c) <- values(fd.c)/fd.max
#
#vals <- round(getValues(fd.c))
#table(vals)
# Aggregate raster total mean
cropharvestRasterAgg <- aggregate(fd.c, fact = resolutionscale, fun=sum, na.action = na.omit) #aggregate raster as specified resolution, function=sum
cropharvestRasterAgg <- cropharvestRasterAgg /resolutionscale / resolutionscale
totalmean <- cropharvestRasterAgg
structure(cropharvestRasterAgg)

# Set pallette for map
palette = colPallett <- c("#F6E726FF","#FEBC2AFF","#F9963FFF", "#EB7557FF", "#D7566CFF","#BE3885FF", "#B32C8EFF","#AD2793FF", "#A11A9CFF", "#8B0AA5FF", "#7C02A8FF", "#7401A8FF", "#6C00A8FF", "#6400A7FF", "#5C01A6FF", "#5C01A6FF", "#5402A3FF","#5402A3FF", "#4B03A1FF", "#43039EFF", "#3A049AFF", "#3A049AFF", "#300597FF", "#300597FF", "#270591FF","#270591FF",  "#1B068DFF","#1B068DFF", "#1B068DFF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF","#0D0887FF")

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland for Southwest Oregon
plot(cropharvestRasterAgg, main=paste('Harvested area fraction based on "total mean" :', 
                                      tree, CellDegree, 'degree PNW area'), 
     col=palette,  xaxt='n',  yaxt='n', axes=F, box=F, xlim=c(latifrom,latito), ylim=c(longifrom,longito))
worldmap <- getMap(resolution = "high")
NorthAmerica <- worldmap[which(worldmap$REGION == "North America"),]
plot(NorthAmerica, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#  9. Land mean, apply inverse power law model and negative exponential model
#-----------------------------------------------------
# 9.1 Apply pcutoff = 0.0015
fddata <- FuncGIScroplandW(cropharvestRasterAgg, pcutoff0) # pcutoff0 = 0.0015, get data
cropharvestRasterAggValues <- getValues(cropharvestRasterAgg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterAgg) > pcutoff0) # get cell number for model
length(cellNumW) # number of pixel
#-----------------------------------------------------
#-----------------------------------------------------
# 9.1a Apply inverse power law model
index328 <- networkbeta(fddata, beta0, cutoffadja0)
index329 <- networkbeta(beta,cutoffadja0)
index330 <- networkbeta(beta1,cutoffadja0)

index331 <- networkbetaW(beta0,cutoffadja)
index332 <- networkbetaW(beta,cutoffadja)
index333 <- networkbetaW(beta1,cutoffadja)

index334 <- networkbetaW(beta0,cutoffadja1)
index335 <- networkbetaW(beta,cutoffadja1)
index336 <- networkbetaW(beta1,cutoffadja1)

