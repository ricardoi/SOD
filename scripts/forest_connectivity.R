# ---
# title: "Forest Inventory National Program CCRI raster 0.? degree"
# author: "R Alcala"
# date: "08/20/2021"
# output: forest invetory map

#----- loading packages
##
library(raster)
library(rgdal)
library(dismo)
library(expm)
library(igraph)
library(maptools)
library(rrcov)
library(splines)
library(rworldmap)
library(mapdata)
library(sp)
library(maps)
library(rgeos)
library(geosphere)
library(RColorBrewer)
library("colorspace")
data(wrld_simpl) #add country boundary to the map
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
## 1.2 Customize crop and values of parameters
##
#-----------------------------------------------------
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
sphere2<-'Western Hemisphere'                      ###
#-----------------------------------------------------
# Range for Southwestern Oregon area               ###
latifrom  <- -124.5 #latitude: from -48 to 76      ###
latito    <- -124                                  ###
longifrom <- -42 #longitude: from -24 to 180       ###
longito   <- -42.5                                 ###
#-----------------------------------------------------
# Range for Western Hemisphere                     ###
longifromW <- -140 #longitude: from -140 to -34    ###
longitoW   <- -120                                  ###
latifromW  <- 28 #latitude: from -58 to 60        ###
latitoW    <- 58                                   ###
#-----------------------------------------------------
# Set map resolution                               ###
resolutionscale <- 6  # 6 of 5-min cells for 0.5 degree ###
CellDegree      <- 0.5                             ###
#-----------------------------------------------------
## Set palette for map                             ###
colPallett <- c("#F6E726FF","#FEBC2AFF","#F9963FFF", "#EB7557FF", "#D7566CFF","#BE3885FF", "#B32C8EFF","#AD2793FF", "#A11A9CFF", "#8B0AA5FF", "#7C02A8FF", "#7401A8FF", "#6C00A8FF", "#6400A7FF", "#5C01A6FF", "#5C01A6FF", "#5402A3FF","#5402A3FF", "#4B03A1FF", "#43039EFF", "#3A049AFF", "#3A049AFF", "#300597FF", "#300597FF", "#270591FF","#270591FF",  "#1B068DFF","#1B068DFF", "#1B068DFF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF", "#0D0887FF","#0D0887FF", "#0D0887FF","#0D0887FF")

palette1<-colPallett                               ###
palette2<-colPallett                               ###
#-----------------------------------------------------
# Set value range for maps                         ###
paldif<-diverge_hcl(12,h=c(128,330),c=98,l=c(65,90))
zr<- range( 0.000000000001, 0.200000001)#Tingting  ###
zr1<- range(0.000000000001, 1.0000000000001)       ###
zrCase <- range (0.000000000001,0.50000000000)     ###
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
## 1.3 Read in a .tif file
#j<-file.choose()
setwd("~/Box/OSU/Maps/mr118_wdycov/")
forestden <- raster("mr118_wdycov/dblbnd.adf")  # data come from LEMMA\GNN

forestden@data@attributes[[1]]

plot(forestden)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')
#-----------------------------------------------------
plot(forestden, ylim=c(latifromW,latitoW), xlim=c(longifromW,longitoW))
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')
#-----------------------------------------------------
zr5min <- range( 0.000000000001, max(getValues(cropharvest), na.rm = T))
plot(cropharvest, main=paste(crop,'cropland 5 min by 5 min map'), col=palette2, zlim= zr5min, xaxt='n',  yaxt='n', axes=F, box=F)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')
#-----------------------------------------------------
# Plot Caribbean area
plot(cropharvest, main=paste(crop,'Caribbean area cropland 5 min by 5 min map'), col=palette2, zlim= zr5min, ylim=c(latifrom,latito), xlim=c(longifrom,longito), xaxt='n',  yaxt='n', axes=F, box=F)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')
#-----------------------------------------------------
#-----------------------------------------------------
```

***
  ***
  # 2. Plot 5min by 5min and aggregated maps for Western Hemisphere
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
# 2.1 Generate crop raster for Western Hemisphere
rowstartW <- round((90-latitoW)/180*nrow(cropharvest))+1 #find the start row number,
numrowW   <- round((latitoW-latifromW)/180*nrow(cropharvest)) # calculate the total row number within latitude range
cropvalueW <- getValues(cropharvest, row=rowstartW,nrows=numrowW)  #extract values from the Map for the range of specified latitude
cropselectW <- matrix(cropvalueW, ncol=ncol(cropharvest), byrow=TRUE)# save data as matrix
colstartW <- (180+longifromW)*12+1  #find the start column number
numcolW <- (longitoW-longifromW)*12 #selected total column number
scolW <- numcolW+colstartW-1  #the end of column number
cropareaW <- cropselectW[,colstartW:scolW] #extract data for the selected area
cropharvestRasterW <- raster(cropareaW, xmn=longifromW, xmx=longitoW, ymn=latifromW, ymx=latitoW, crs= "+proj=longlat +datum=WGS84")

#-----------------------------------------------------
#-----------------------------------------------------
# 2.2 Plot 5'by 5' maps Western Hemisphere
plot(cropharvestRasterW)
plot(wrld_simpl, add=TRUE)
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')
#-----------------------------------------------------
plot(cropharvestRasterW, main=paste(sphere2, crop,'cropland 5 min by 5 min map'), col=palette2, zlim=zr5min )
plot(wrld_simpl,add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')
#-----------------------------------------------------
plot(cropharvestRasterW, main=paste(sphere2, crop,'cropland 5 min by 5 min map'), col=palette2, zlim=zr5min, ylim=c(latifrom,latito), xlim=c(longifrom,longito))
plot(wrld_simpl,add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#-----------------------------------------------------
#-----------------------------------------------------
```

***
  ***
  #  3.  Aggregate raster as specified resolution, Total Mean
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
# Aggregate raster total mean
cropharvestRasterWagg <- aggregate(cropharvestRasterW, fact = resolutionscale, fun=sum, na.action = na.omit ) #aggregate raster as specified resolution, function=sum
cropharvestRasterWagg <- cropharvestRasterWagg /resolutionscale /resolutionscale
totalmeanW <- cropharvestRasterWagg
structure(cropharvestRasterWagg)

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland
zrAggTmean <- range( 0.000000000001, max(getValues(totalmeanW), na.rm = TRUE))
plot(cropharvestRasterWagg, main=paste('Harvested area fraction based on "total mean" :', crop, CellDegree, 'degree'), col=palette2, zlim= zrAggTmean,  xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland for Caribbean area
plot(cropharvestRasterWagg, main=paste('Harvested area fraction based on "total mean" :', crop, CellDegree, 'degree Caribbean area'), col=palette2, zlim= zrAggTmean,  xaxt='n',  yaxt='n', axes=F, box=F, ylim=c(latifrom,latito), xlim=c(longifrom,longito))
plot(countriesLow, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#-----------------------------------------------------
#-----------------------------------------------------

```

***
  ***
  #  4.  Function : generate data frame with GIS info. and values of harvest cropland fraction
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------

# 4.1 Function to generate data frame with GIS info. and values of harvest cropland fraction
FuncGIScroplandW <- function(cropRasterW, HCFcutoff){
  cropRasterWValues <- getValues(cropRasterW)
  WcellNum <- which(cropRasterWValues > HCFcutoff)
  cellNumValueW <- cropRasterWValues[WcellNum]
  Wlength <- length(cellNumValueW)
  colnumsW <- ncol(cropRasterW)
  latiW <- c(rep(0, Wlength))
  logiW <- c(rep(0, Wlength))
  for (k in c(1:Wlength)) {
    me<-(WcellNum[k]-1)%/%colnumsW
    latiW[k]<-latitoW-me*CellDegree
    logiW[k]<-(WcellNum[k]-1-me*colnumsW)*CellDegree+longifromW
  }
  cropdataW <- cbind(latiW, logiW, cellNumValueW)
  return(as.data.frame( cropdataW))
}

#-----------------------------------------------------
#-----------------------------------------------------


```

***
  ***
  #  5. Inverse Power Law Model
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
networkbetaW <- function(beta3, cutoffadja3){
  ##############################################
  #### create adjacency matrix
  ####
  rownumber13 <- nrow(cropdataW)
  latilongimatr3 <- cropdataW[1:rownumber13,c(2,1)]# save the latitude and longitude as new matrix
  #---- use Geosphere package, function distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
  dvse <- distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree

  latilongimatr3 <- as.matrix(latilongimatr3)
  TemMat <- matrix(-999, nrow( latilongimatr3),nrow(latilongimatr3))

  for (i in 1:nrow(latilongimatr3)) {
    TemMat[i, ] <- distVincentyEllipsoid(latilongimatr3[i,], latilongimatr3)/dvse
  }

  distancematr3 <- TemMat
  #---- end of code

  distancematrexp3 <- distancematr3^(-beta3)  #use function C=AX^(-beta), here A=1, X=distancematr3
  cropmatr3 <- cropdataW[1:rownumber13,3] # complete gravity model with crop data
  cropmatr13 <- matrix(cropmatr3,,1)
  cropmatr23 <- matrix(cropmatr3,1,)
  cropmatrix3 <- cropmatr13%*%cropmatr23
  cropmatrix3 <- as.matrix( cropmatrix3)
  cropdistancematr3 <- distancematrexp3* cropmatrix3
  logicalmatr3 <- cropdistancematr3>cutoffadja3
  stan3 <- cropdistancematr3*logicalmatr3
  cropdistancematrix3 <- graph.adjacency(stan3,mode=c("undirected"),diag=FALSE, weighted=TRUE)#change the thresh to see the difference
  ##############################################
  #### create network for all the selected nodes
  ####
  #V(cropdistancematrix3)$color=colororder3
  V(cropdistancematrix3)$label.cex=0.7
  E(cropdistancematrix3)$color="red"
  edgeweight3<-E(cropdistancematrix3)$weight*10000
  #plot(cropdistancematrix3,vertex.size=povalue3*500,edge.arrow.size=0.2,edge.width=edgeweight3,vertex.label=NA,main=paste(crop,sphere2, 'adjacency matrix threshold>',cutoffadja3, ', beta=',beta3)) # network with weighted node sizes
  #plot(cropdistancematrix3,vertex.size=10,edge.arrow.size=0.2,edge.width=edgeweight3,vertex.label=NA,main=paste(crop,sphere2, 'adjacency matrix threshold>',cutoffadja3, ', beta=',beta3)) # network with identical node size
  knnpref0<-graph.knn(cropdistancematrix3,weights=NA)$knn
  knnpref0[is.na(knnpref0)]<-0
  degreematr<-degree(cropdistancematrix3)
  knnpref<-knnpref0*degreematr
  if(max(knnpref)==0){knnprefp=0}else
    if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}

  ##############################################
  ####  node degree, node strengh
  ####
  nodestrength <- graph.strength(cropdistancematrix3)
  nodestrength[is.na(nodestrength)] <- 0
  if(max(nodestrength)==0){nodestr=0}else
    if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/6}
  ##############################################
  ####  betweenness centrality
  ####
  between <- betweenness(cropdistancematrix3)
  between[is.na(between)] <- 0
  if(max(between)==0){betweenp=0}else
    if(max(between)>0){betweenp=between/max(between)/2}
  ##############################################
  ####   eigenvector and eigenvalues
  ####
  eigenvectorvalues<-evcent(cropdistancematrix3)
  ev<-eigenvectorvalues$vector
  ev[is.na(ev)]<-0
  if(max(ev)==0){evp=0}else
    if(max(ev)!=0){evp=ev/max(ev)/6}
  ##############################################
  ####   plot index layer
  ####
  index<-knnprefp+evp+betweenp+nodestr
  indexpre<- cropharvestRasterWaggValues
  indexpre[]<-0
  indexpre[cellNumW]<-index
  indexv3<-indexpre
  return(indexv3)
}
#-----------------------------------------------------
#-----------------------------------------------------
```
***
  ***
  #  6. Negative Exponential Model
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
networkgammaW <- function(gamma3,cutoffadja3){
  ##############################################
  #### create adjacency matrix
  ####
  rownumber13 <- nrow(cropdataW)
  latilongimatr3 <- cropdataW[1:rownumber13,c(2,1)]# save the latitude and longitude as new matrix  #---- use Geosphere package, function distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
  dvse <- distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree

  latilongimatr3 <- as.matrix(latilongimatr3)
  TemMat <- matrix(-999, nrow( latilongimatr3),nrow(latilongimatr3))

  for (i in 1:nrow(latilongimatr3)) {
    TemMat[i, ] <- distVincentyEllipsoid(latilongimatr3[i,], latilongimatr3)/dvse
  }

  distancematr3 <- TemMat
  #---- end of code

  eulernumber<-exp(1)
  distancematrexponential3<-eulernumber^(-gamma3*distancematr3)# exponential model

  cropmatr3<-cropdataW[1:rownumber13,3] # complete gravity model with crop data
  cropmatr13<-matrix(cropmatr3,,1)
  cropmatr23<-matrix(cropmatr3,1,)
  cropmatrix3<-cropmatr13%*%cropmatr23
  cropmatrix3<-as.matrix( cropmatrix3)
  cropdistancematr3<-distancematrexponential3* cropmatrix3
  logicalmatr3<-cropdistancematr3>cutoffadja3
  stan3<-cropdistancematr3*logicalmatr3
  cropdistancematrix3<-graph.adjacency(stan3,mode=c("undirected"),diag=FALSE, weighted=TRUE)#change the thresh to see the difference
  ##############################################
  #### create network for all the selected nodes
  ####
  #V(cropdistancematrix3)$color=colororder3
  V(cropdistancematrix3)$label.cex=0.7
  E(cropdistancematrix3)$color="red"
  edgeweight3<-E(cropdistancematrix3)$weight*10000
  #plot(cropdistancematrix3,vertex.size=povalue3*500,edge.arrow.size=0.2,edge.width=edgeweight3,vertex.label=NA,main=paste(crop,sphere2, 'adjacency matrix threshold>',cutoffadja3, ', beta=',beta3)) # network with weighted node sizes
  #plot(cropdistancematrix3,vertex.size=10,edge.arrow.size=0.2,edge.width=edgeweight3,vertex.label=NA,main=paste(crop,sphere2, 'adjacency matrix threshold>',cutoffadja3, ', beta=',beta3)) # network with identical node size
  knnpref0<-graph.knn(cropdistancematrix3,weights=NA)$knn
  knnpref0[is.na(knnpref0)]<-0
  degreematr<-degree(cropdistancematrix3)
  knnpref<-knnpref0*degreematr
  if(max(knnpref)==0){knnprefp=0}else
    if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}

  ##############################################
  ####  node degree, node strengh
  ####
  nodestrength<-graph.strength(cropdistancematrix3)
  nodestrength[is.na(nodestrength)]<-0
  if(max(nodestrength)==0){nodestr=0}else
    if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/6}
  ##############################################
  ####  betweenness centrality
  ####
  between<-betweenness(cropdistancematrix3)
  between[is.na(between)]<-0
  if(max(between)==0){betweenp=0}else
    if(max(between)>0){betweenp=between/max(between)/2}
  ##############################################
  ####   eigenvector and eigenvalues
  ####
  eigenvectorvalues<-evcent(cropdistancematrix3)
  ev<-eigenvectorvalues$vector
  ev[is.na(ev)]<-0
  if(max(ev)==0){evp=0}else
    if(max(ev)!=0){evp=ev/max(ev)/6}
  ##############################################
  ####   plot index layer
  ####
  index<-knnprefp+evp+betweenp+nodestr
  indexpre <- cropharvestRasterWaggValues
  indexpre[]<-0
  indexpre[cellNumW]<-index
  indexv3<-indexpre
  return(indexv3)

}
#-----------------------------------------------------
#-----------------------------------------------------

```


***
  ***
  #  7. Total mean, apply inverse power law model and negative exponential model
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
# 7.1 Apply pcutoff = 0.0015
cropdataW <- FuncGIScroplandW(cropharvestRasterWagg, pcutoff0) # pcutoff0 = 0.0015, get data
cropharvestRasterWaggValues <- getValues(cropharvestRasterWagg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterWagg) > pcutoff0) # get cell number for model
length(cellNumW) # number of pixel
#-----------------------------------------------------
#-----------------------------------------------------
# 7.1a Apply inverse power law model
index301 <- networkbetaW(beta0,cutoffadja0) # it takes aobut 4 mins to run
index302 <- networkbetaW(beta,cutoffadja0)
index303 <- networkbetaW(beta1,cutoffadja0)

index304 <- networkbetaW(beta0,cutoffadja)
index305 <- networkbetaW(beta,cutoffadja)
index306 <- networkbetaW(beta1,cutoffadja)

index307 <- networkbetaW(beta0,cutoffadja1)
index308 <- networkbetaW(beta,cutoffadja1)
index309 <- networkbetaW(beta1,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
# 7.1b Apply negative exponential model
index01exm <- networkgammaW(gamma00,cutoffadja0)
index02exm <- networkgammaW(gamma0,cutoffadja0)
index03exm <- networkgammaW(gamma,cutoffadja0)
index04exm <- networkgammaW(gamma1,cutoffadja0)
index05exm <- networkgammaW(gamma2,cutoffadja0)

index06exm <- networkgammaW(gamma00,cutoffadja)
index07exm <- networkgammaW(gamma0,cutoffadja)
index08exm <- networkgammaW(gamma,cutoffadja)
index09exm <- networkgammaW(gamma1,cutoffadja)
index10exm <- networkgammaW(gamma2,cutoffadja)

index11exm <- networkgammaW(gamma00,cutoffadja1)
index12exm <- networkgammaW(gamma0,cutoffadja1)
index13exm <- networkgammaW(gamma,cutoffadja1)
index14exm <- networkgammaW(gamma1,cutoffadja1)
index15exm <- networkgammaW(gamma2,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
# 7.2 Apply pcutoff = 0.002
cropdataW <- FuncGIScroplandW(cropharvestRasterWagg, pcutoff) # pcutoff0 = 0.002, get data
cropharvestRasterWaggValues <- getValues(cropharvestRasterWagg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterWagg) > pcutoff) # get cell number for model
length(cellNumW) # number of pixel
#-----------------------------------------------------
#-----------------------------------------------------
# 7.2a Apply inverse power law model
index310 <- networkbetaW(beta0,cutoffadja0)
index311 <- networkbetaW(beta,cutoffadja0)
index312 <- networkbetaW(beta1,cutoffadja0)

index313 <- networkbetaW(beta0,cutoffadja)
index314 <- networkbetaW(beta,cutoffadja)
index315 <- networkbetaW(beta1,cutoffadja)

index316 <- networkbetaW(beta0,cutoffadja1)
index317 <- networkbetaW(beta,cutoffadja1)
index318 <- networkbetaW(beta1,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
# 7.2b Apply negative exponential model
index16exm <- networkgammaW(gamma00,cutoffadja0)
index17exm <- networkgammaW(gamma0,cutoffadja0)
index18exm <- networkgammaW(gamma,cutoffadja0)
index19exm <- networkgammaW(gamma1,cutoffadja0)
index20exm <- networkgammaW(gamma2,cutoffadja0)

index21exm <- networkgammaW(gamma00,cutoffadja)
index22exm <- networkgammaW(gamma0,cutoffadja)
index23exm <- networkgammaW(gamma,cutoffadja)
index24exm <- networkgammaW(gamma1,cutoffadja)
index25exm <- networkgammaW(gamma2,cutoffadja)

index26exm <- networkgammaW(gamma00,cutoffadja1)
index27exm <- networkgammaW(gamma0,cutoffadja1)
index28exm <- networkgammaW(gamma,cutoffadja1)
index29exm <- networkgammaW(gamma1,cutoffadja1)
index30exm <- networkgammaW(gamma2,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
# 7.3 Apply pcutoff = 0.0025
cropdataW <- FuncGIScroplandW(cropharvestRasterWagg, pcutoff1) # pcutoff0 = 0.0025, get data
cropharvestRasterWaggValues <- getValues(cropharvestRasterWagg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterWagg) > pcutoff1) # get cell number for model
length(cellNumW) # number of pixel
#-----------------------------------------------------
#-----------------------------------------------------
# 7.3a Apply inverse power law model
index319 <- networkbetaW(beta0,cutoffadja0)
index320 <- networkbetaW(beta,cutoffadja0)
index321 <- networkbetaW(beta1,cutoffadja0)

index322 <- networkbetaW(beta0,cutoffadja)
index323 <- networkbetaW(beta,cutoffadja)
index324 <- networkbetaW(beta1,cutoffadja)

index325 <- networkbetaW(beta0,cutoffadja1)
index326 <- networkbetaW(beta,cutoffadja1)
index327 <- networkbetaW(beta1,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
# 7.3b Apply negative exponential model
index31exm <- networkgammaW(gamma00,cutoffadja0)
index32exm <- networkgammaW(gamma0,cutoffadja0)
index33exm <- networkgammaW(gamma,cutoffadja0)
index34exm <- networkgammaW(gamma1,cutoffadja0)
index35exm <- networkgammaW(gamma2,cutoffadja0)

index36exm <- networkgammaW(gamma00,cutoffadja)
index37exm <- networkgammaW(gamma0,cutoffadja)
index38exm <- networkgammaW(gamma,cutoffadja)
index39exm <- networkgammaW(gamma1,cutoffadja)
index40exm <- networkgammaW(gamma2,cutoffadja)

index41exm <- networkgammaW(gamma00,cutoffadja1)
index42exm <- networkgammaW(gamma0,cutoffadja1)
index43exm <- networkgammaW(gamma,cutoffadja1)
index44exm <- networkgammaW(gamma1,cutoffadja1)
index45exm <- networkgammaW(gamma2,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------

```
***
  ***
  #  8.  Aggregate raster as specified resolution, Land Mean
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
cropharvestRasterWagg <- aggregate(cropharvestRasterW, fact = resolutionscale, fun=mean, na.action = na.omit ) #aggregate raster as specified resolution, function=sum
landmeanW <- cropharvestRasterWagg
structure(cropharvestRasterWagg)

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland
zrAggTmean <- range( 0.000000000001, max(getValues(totalmeanW), na.rm = TRUE))
plot(cropharvestRasterWagg, main=paste('Harvested area fraction based on "land mean" :', crop, CellDegree, 'degree'), col=palette2, zlim= zrAggTmean,  xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')

#-----------------------------------------------------
# Plot aggregated raster of harvested area of cropland for Caribbean area
plot(cropharvestRasterWagg, main=paste('Harvested area fraction based on "land mean" :', crop, CellDegree, 'degree Caribbean area'), col=palette2, zlim= zrAggTmean,  xaxt='n',  yaxt='n', axes=F, box=F, ylim=c(latifrom,latito), xlim=c(longifrom,longito))
plot(countriesLow, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='#31688EFF', boundary='black')

#-----------------------------------------------------
#-----------------------------------------------------

```

***
  ***
  #  9. Land mean, apply inverse power law model and negative exponential model
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
# 9.1 Apply pcutoff = 0.0015
cropdataW <- FuncGIScroplandW(cropharvestRasterWagg, pcutoff0) # pcutoff0 = 0.0015, get data
cropharvestRasterWaggValues <- getValues(cropharvestRasterWagg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterWagg) > pcutoff0) # get cell number for model
length(cellNumW) # number of pixel
#-----------------------------------------------------
#-----------------------------------------------------
# 9.1a Apply inverse power law model
index328 <- networkbetaW(beta0,cutoffadja0)
index329 <- networkbetaW(beta,cutoffadja0)
index330 <- networkbetaW(beta1,cutoffadja0)

index331 <- networkbetaW(beta0,cutoffadja)
index332 <- networkbetaW(beta,cutoffadja)
index333 <- networkbetaW(beta1,cutoffadja)

index334 <- networkbetaW(beta0,cutoffadja1)
index335 <- networkbetaW(beta,cutoffadja1)
index336 <- networkbetaW(beta1,cutoffadja1)


#-----------------------------------------------------
#-----------------------------------------------------
# 9.1b Apply negative exponential model

index46exm <- networkgammaW(gamma00,cutoffadja0)
index47exm <- networkgammaW(gamma0,cutoffadja0)
index48exm <- networkgammaW(gamma,cutoffadja0)
index49exm <- networkgammaW(gamma1,cutoffadja0)
index50exm <- networkgammaW(gamma2,cutoffadja0)

index51exm <- networkgammaW(gamma00,cutoffadja)
index52exm <- networkgammaW(gamma0,cutoffadja)
index53exm <- networkgammaW(gamma,cutoffadja)
index54exm <- networkgammaW(gamma1,cutoffadja)
index55exm <- networkgammaW(gamma2,cutoffadja)

index56exm <- networkgammaW(gamma00,cutoffadja1)
index57exm <- networkgammaW(gamma0,cutoffadja1)
index58exm <- networkgammaW(gamma,cutoffadja1)
index59exm <- networkgammaW(gamma1,cutoffadja1)
index60exm <- networkgammaW(gamma2,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
# 9.2 Apply pcutoff = 0.002
cropdataW <- FuncGIScroplandW(cropharvestRasterWagg, pcutoff) # pcutoff0 = 0.002, get data
cropharvestRasterWaggValues <- getValues(cropharvestRasterWagg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterWagg) > pcutoff) # get cell number for model
length(cellNumW) # number of pixel
#-----------------------------------------------------
#-----------------------------------------------------
# 9.2a Apply inverse power law model

index337 <- networkbetaW(beta0,cutoffadja0)
index338 <- networkbetaW(beta,cutoffadja0)
index339 <- networkbetaW(beta1,cutoffadja0)

index340 <- networkbetaW(beta0,cutoffadja)
index341 <- networkbetaW(beta,cutoffadja)
index342 <- networkbetaW(beta1,cutoffadja)

index343 <- networkbetaW(beta0,cutoffadja1)
index344 <- networkbetaW(beta,cutoffadja1)
index345 <- networkbetaW(beta1,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
# 9.2b Apply negative exponential model

index61exm <- networkgammaW(gamma00,cutoffadja0)
index62exm <- networkgammaW(gamma0,cutoffadja0)
index63exm <- networkgammaW(gamma,cutoffadja0)
index64exm <- networkgammaW(gamma1,cutoffadja0)
index65exm <- networkgammaW(gamma2,cutoffadja0)

index66exm <- networkgammaW(gamma00,cutoffadja)
index67exm <- networkgammaW(gamma0,cutoffadja)
index68exm <- networkgammaW(gamma,cutoffadja)
index69exm <- networkgammaW(gamma1,cutoffadja)
index70exm <- networkgammaW(gamma2,cutoffadja)

index71exm <- networkgammaW(gamma00,cutoffadja1)
index72exm <- networkgammaW(gamma0,cutoffadja1)
index73exm <- networkgammaW(gamma,cutoffadja1)
index74exm <- networkgammaW(gamma1,cutoffadja1)
index75exm <- networkgammaW(gamma2,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
# 9.3 Apply pcutoff = 0.0025
cropdataW <- FuncGIScroplandW(cropharvestRasterWagg, pcutoff1) # pcutoff0 = 0.0025, get data
cropharvestRasterWaggValues <- getValues(cropharvestRasterWagg) # get raster values for model
cellNumW <- which(getValues(cropharvestRasterWagg) > pcutoff1) # get cell number for model
length(cellNumW) # number of pixel

#-----------------------------------------------------
#-----------------------------------------------------
# 9.3a Apply inverse power law model
index346 <- networkbetaW(beta0,cutoffadja0)
index347 <- networkbetaW(beta,cutoffadja0)
index348 <- networkbetaW(beta1,cutoffadja0)


index349 <- networkbetaW(beta0,cutoffadja)
index350 <- networkbetaW(beta,cutoffadja)
index351 <- networkbetaW(beta1,cutoffadja)


index352 <- networkbetaW(beta0,cutoffadja1)
index353 <- networkbetaW(beta,cutoffadja1)
index354 <- networkbetaW(beta1,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------
# 9.3b Apply negative exponential model
index76exm <- networkgammaW(gamma00,cutoffadja0)
index77exm <- networkgammaW(gamma0,cutoffadja0)
index78exm <- networkgammaW(gamma,cutoffadja0)
index79exm <- networkgammaW(gamma1,cutoffadja0)
index80exm <- networkgammaW(gamma2,cutoffadja0)

index81exm <- networkgammaW(gamma00,cutoffadja)
index82exm <- networkgammaW(gamma0,cutoffadja)
index83exm <- networkgammaW(gamma,cutoffadja)
index84exm <- networkgammaW(gamma1,cutoffadja)
index85exm <- networkgammaW(gamma2,cutoffadja)

index86exm <- networkgammaW(gamma00,cutoffadja1)
index87exm <- networkgammaW(gamma0,cutoffadja1)
index88exm <- networkgammaW(gamma,cutoffadja1)
index89exm <- networkgammaW(gamma1,cutoffadja1)
index90exm <- networkgammaW(gamma2,cutoffadja1)

#-----------------------------------------------------
#-----------------------------------------------------

```
***
  ***
  #  10. Save CCRI raster
  ***
  ***
  ```{r fig.width=10, fig.height=7, dpi=100}
#-----------------------------------------------------
#-----------------------------------------------------
# Uncertainty matrix
westernriskindexmatr <- cbind(index301,index302,index303,index304,index305,index306,index307,index308,index309,index310,index311,index312,index313,index314,index315,index316,index317,index318,index319,index320,index321,index322,index323,index324,index325,index326,index327,index328,index329,index330,index331,index332,index333,index334,index335,index336,index337,index338,index339,index340,index341,index342,index343,index344,index345,index346,index347,index348,index349,index350,index351,index352,index353,index354,index01exm,index02exm,index03exm,index04exm,index05exm,index06exm,index07exm,index08exm,index09exm,index10exm,index11exm,index12exm,index13exm,index14exm,index15exm,index16exm,index17exm,index18exm,index19exm,index20exm,index21exm,index22exm,index23exm,index24exm,index25exm,index26exm,index27exm,index28exm,index29exm,index30exm,index31exm,index32exm,index33exm,index34exm,index35exm,index36exm,index37exm,index38exm,index39exm,index40exm,index41exm,index42exm,index43exm,index44exm,index45exm,index46exm,index47exm,index48exm,index49exm,index50exm,index51exm,index52exm,index53exm,index54exm,index55exm,index56exm,index57exm,index58exm,index59exm,index60exm,index61exm,index62exm,index63exm,index64exm,index65exm,index66exm,index67exm,index68exm,index69exm,index70exm,index71exm,index72exm,index73exm,index74exm,index75exm,index76exm,index77exm,index78exm,index79exm,index80exm,index81exm,index82exm,index83exm,index84exm,index85exm,index86exm,index87exm,index88exm,index89exm,index90exm)

#-----------------------------------------------------
#-----------------------------------------------------
# Plot Mean CCRI
meanindexW <- apply(westernriskindexmatr,1,mean)
meanmapsW <- matrix(meanindexW,nrow(totalmeanW),ncol(totalmeanW),byrow=TRUE)
meanmapsrasterW<-raster(meanmapsW,xmn=longifromW,xmx=longitoW,ymn=latifromW,ymx=latitoW)
plot(meanmapsrasterW,main=paste('Mean cropland connectivity risk index from sensitivity analysis: ',  crop, CellDegree,'degree'), col=palette1,zlim= range(0.00000000001, max(meanindexW)), xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)

#-----------------------------------------------------
# save ccri mean raster
#setwd("~/Dropbox (UFL)/168/2018/2018 11 Caribbean cropland connectivity/2 code/Code of Monfreda/Monfreda Caribbean Code Geoshpere produce CCRI raster/Monfreda Caribbean Code Geoshpere produce CCRI raster 0.5 degree")
maintitleMean <- paste( crop, "MapSpam Mean CCRI WH half degree.tif")
#writeRaster( meanmapsrasterW , maintitleMean, overwrite=TRUE)
#plot(meanmapsrasterW)
#--------------- plot caribbean area
plot(meanmapsrasterW,main=paste('Mean cropland connectivity risk index from sensitivity analysis: ',  crop, 'Caribbean area',CellDegree, 'degree'), col=palette1,zlim= range(0.00000000001, max(meanindexW)), xaxt='n',  yaxt='n', axes=F, box=F, ylim=c(latifrom,latito),xlim=c(longifrom,longito))
plot(countriesLow, add=TRUE)

#--------------- plot Haiti area
plot(meanmapsrasterW,main=paste('Mean cropland connectivity risk index from sensitivity analysis: ',  crop, 'Haiti area',CellDegree, 'degree'), col=palette1,zlim= range(0.00000000001, max(meanindexW)), xaxt='n',  yaxt='n', axes=F, box=F, ylim=c(10,20),xlim=c(-80,-20))
plot(countriesLow, add=TRUE)

#-----------------------------------------------------
#-----------------------------------------------------
# Plot Max CCRI

maxindexW <- apply(westernriskindexmatr,1,max)
maxmapsW <- matrix(maxindexW,nrow(totalmeanW),ncol(totalmeanW),byrow=TRUE)
maxmapsrasterW <- raster(maxmapsW,xmn=longifromW,xmx=longitoW,ymn=latifromW,ymx=latitoW)
plot(maxmapsrasterW, main=paste('Maximum cropland connectivity risk index from sensitivity analysis:', crop, CellDegree,'degree'), col=palette1,zlim= range(0.00000000001, max(maxindexW)), xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)

maintitleMax <- paste( crop, "MapSpam Max CCRI WH half degree.tif")
#writeRaster( maxmapsrasterW  , maintitleMax , overwrite=TRUE)


#-----------------------------------------------------
#-----------------------------------------------------
# Plot Min CCRI
minindexW <- apply(westernriskindexmatr,1,min)
minmapsW <- matrix(minindexW,nrow(totalmeanW),ncol(totalmeanW),byrow=TRUE)
minmapsrasterW <- raster(minmapsW,xmn=longifromW,xmx=longitoW,ymn=latifromW,ymx=latitoW)
zrminw<-range(0.000000000001,max(minindexW))
plot(minmapsrasterW,main=paste('Minimum cropland connectivity risk index from sensitivity analysis:', crop, CellDegree,'degree' ), col=palette1,zlim=zrminw, xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)

maintitleMin <- paste( crop, "MapSpam Min CCRI WH half degree.tif")
#writeRaster( minmapsrasterW, maintitleMin, overwrite=TRUE)
#-----------------------------------------------------
#-----------------------------------------------------
# Plot Var CCRI

varindexW <- apply(westernriskindexmatr,1,var)
varmapsW <- matrix(varindexW,nrow(totalmeanW),ncol(totalmeanW),byrow=TRUE)
varmapsrasterW <- raster(varmapsW,xmn=longifromW,xmx=longitoW,ymn=latifromW,ymx=latitoW)
plot(varmapsrasterW,main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', crop, CellDegree,'degree'), col=palette1,zlim=range(0.000000000001,max(varindexW)), xaxt='n',  yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)

maintitleVar <- paste( crop, "MapSpam Var CCRI WH half degree.tif")
#writeRaster( varmapsrasterW, maintitleVar, overwrite=TRUE)

#-----------------------------------------------------
#-----------------------------------------------------
maintitleTotalmean <- paste( crop, "MapSpam totalmean WH half degree.tif")
#writeRaster( totalmeanW, maintitleTotalmean, overwrite=TRUE)
maintitleLandmean <- paste( crop, "MapSpam landmean WH half degree.tif")
#writeRaster( landmeanW, maintitleLandmean, overwrite=TRUE)
#-----------------------------------------------------
#-----------------------------------------------------
```




