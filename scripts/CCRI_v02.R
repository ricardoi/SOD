# title: "Cropland connectivity"
# author: Yanru Xing @GarretLab
# adapted: Ricardo I Alcala
# date: "`r format(Sys.time(), '%d %B, %Y')`"
#
# output:
#   html_document:
#     toc: true
#     toc_depth: 2
#     toc_float: true
#     number_sections: true
#     df_print: paged
#----------------------------------------------
# Load packages
library(igraph)
library(tidyverse)
library(viridis)
library(scales)
library(rgdal)
library(rworldmap)
library(sf)
library(tmap)
library(raster)
library(INA)
library(maptools)
library(maps)

# setting working directory
setwd("~/Documents/Maps/Tree_species/")
#---------------------------------------------------------------
# load raster
forestden <- raster("rasters/lide3_ba_2017.tif")
fd <- projectRaster(forestden,
                    crs ="+proj=longlat +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") #utm
plot(fd)
#---------------------------------------------------------------
# transforming proj = aea (Azimutal equal area) to UWT
# Range for Southwestern Oregon area               ###
latifrom  <- -124.5 #latitude: from -48 to 76      ###
latito    <- -123.7                                ###
longifrom <- 42 #longitude: from -24 to 180        ###
longito   <- 42.5
#---------------------------------------------------------------
# creating color palette
palette1 <- rev(viridis_pal(option = "B")(1000))
colid<-c(1:100)*10
colid <- c(1, colid )
palette1<-palette1[colid]
#----------------------------------------------

colPalletNew <- c( "#F4E156FF", "#F6D746FF", "#F8CD37FF", "#FAC329FF", "#FBB91EFF", "#FCAF13FF",
                   "#FCA50BFF", "#FB9C06FF", "#FA9207FF", "#F8890CFF", "#F68013FF", "#F37819FF",
                   "#F06F20FF", "#EC6727FF", "#E85F2EFF", "#E25834FF", "#DD5139FF", "#D74B3FFF",
                   "#D04545FF", "#CA404AFF", "#C33B4FFF", "#BC3754FF", "#B43359FF", "#AC305EFF",
                   "#A42C60FF", "#9B2964FF", "#932667FF", "#922568FF", "#902568FF", "#8F2469FF",
                   "#8D2369FF", "#8C2369FF", "#8A226AFF", "#88226AFF", "#87216BFF", "#85216BFF",
                   "#84206BFF", "#82206CFF", "#801F6CFF", "#7F1E6CFF", "#7D1E6DFF", "#7C1D6DFF",
                   "#7A1D6DFF", "#781C6DFF", "#771C6DFF", "#751B6EFF", "#741A6EFF", "#721A6EFF",
                   "#71196EFF", "#6E196EFF", "#6D186EFF", "#6B186EFF", "#6A176EFF", "#68166EFF",
                   "#66166EFF", "#65156EFF", "#63156EFF", "#61136EFF", "#60136EFF", "#5E126EFF",
                   "#5C126EFF", "#5B126EFF", "#59106EFF", "#58106EFF", "#560F6DFF", "#540F6DFF",
                   "#530E6DFF", "#510E6CFF", "#500D6CFF", "#4D0D6CFF", "#4C0C6BFF", "#4A0C6BFF",
                   "#490B6AFF", "#470B6AFF", "#450A69FF", "#440A68FF", "#420A68FF", "#400A67FF",
                   "#3E0966FF", "#3D0965FF", "#3B0964FF", "#390963FF", "#380962FF", "#360961FF",
                   "#340A5FFF", "#320A5EFF", "#310A5CFF", "#2F0A5BFF", "#2D0B59FF", "#2B0B57FF",
                   "#290B55FF", "#280B53FF", "#250C51FF", "#240C4EFF", "#230C4BFF", "#200C49FF",
                   "#1F0C47FF", "#1D0C44FF", "#1C0C42FF", "#1A0C40FF", "#190C3DFF", "#170C3BFF",
                   "#150B38FF", "#150B36FF", "#130A33FF", "#110A31FF", "#11092EFF", "#0F092CFF",
                   "#0D082AFF", "#0C0827FF", "#0B0725FF", "#0A0723FF", "#090620FF", "#08051EFF",
                   "#07051CFF", "#060419FF", "#050418FF", "#040315FF", "#040312FF", "#030210FF",
                   "#02020EFF", "#02020CFF", "#02010AFF", "#010108FF", "#010106FF", "#010005FF",
                   "#000004FF", "#000004FF", "#000004FF")


#-----------------------------------------------------
## Crop cropland area to the desired extent, then plot
fd.c <- crop(fd, extent(latifrom, latito, longifrom, longito))

worldmap <- getMap(resolution = "high")
NorthAmerica <- worldmap[which(worldmap$REGION == "North America"),]
plot(fd.c)
plot(NorthAmerica, add=TRUE)  #add country boundary to the map
 map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

# s.sf_Burundi <- st_read("bdi_admbnda_adm0_igebu_ocha_itos_20171103.shp")
# potato_bdi <- crop(PotatoAFRICA, s.sf_Burundi)
#
# s.sf_Rwanda <- st_read("rwa_adm0_2006_NISR_WGS1984_20181002.shp")
# potato_rwa <- crop(PotatoAFRICA, s.sf_Rwanda)
#
# potato_bdi_rwa <- merge(potato_bdi, potato_rwa)
# plot(potato_bdi_rwa)
# plot(countriesLow, add = TRUE)
# map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')

#------------------------------------------------------------
# normalizng data
library(tidyverse)
values(fd.c)[values(fd.c) < 0] <- 0
#values(fd.c) <- na_if(values(fd.c), 0)
values(fd.c)[is.na(values(fd.c))] <- 0
values(fd.c)

# Normalizing the data values
 
# NO MISSING VALUES HERE

fd.max <- max(na.omit((values(fd.c)))) # thats my vector of densities, location will be change
values(fd.c) <- values(fd.c)/fd.max
fd.v <- getValues(fd.c)
#fd.v[!fd.v > 0] <- 0   #which(fd.v > 0)
#fd.v[is.na(fd.v)] <- 0
cropValue <- fd.v
length(fd.v)

#-------------------------------------------------------------
# cropValue <- fd.v
# fd.AggID <- which(fd.v > 0)
# cell_id <- fd.AggID
# length(fd.AggID)
#writeRaster(potatoEthAgg, "potato_bdi_rwa_5min_cropland_density.tif")

#-------------------------------------
#agreggate
fd.Agg <- aggregate(fd.c, fact = 32, fun=sum, na.action = na.omit) #aggregate raster as specified resolution, function=sum
fd.Aggv <- getValues(fd.Agg)
fd.AggID <- which(fd.Aggv > 0)
cell_id <- fd.AggID
length(fd.AggID)
# NO PUEDO TENER NEGATIVOS en fd.Agg


plot(fd.Agg)
plot(NorthAmerica, add=TRUE)  #add country boundary to the map
map('lakes', add=TRUE, fill=TRUE, col='skyblue', boundary='black')


#--------------------------------------
Xmin <- NULL
Xmax <- NULL
Ymin <- NULL
Ymax <- NULL

for (i in 1:length(fd.AggID)) {
  print(i)
  extCells <- extentFromCells(fd.c, fd.AggID[i])
  Xmin = c(Xmin, extCells[1])
  Xmax = c(Xmax, extCells[2])
  Ymin = c(Ymin, extCells[3])
  Ymax = c(Ymax, extCells[4])
}
#-------------------------------------------
fd_land <- cropValue[fd.AggID]

points_crop <- data.frame(Xmin, Ymax, fd_land)
#write.csv(points_crop, "Potato BDI RWA GIS and cropland 5min.csv" )
cropValue <- fd_land

# Distance matrix
distancematr <-
  dist(points_crop[ , c(1:2)],
  method = 'euclidean',
  diag = T,
  upper = T)

#View(distancematr)

distance_matrix <- as.matrix(distancematr)
GIS_pepper_sub<-points_crop[ , c(1:2)]

lon <- GIS_pepper_sub$Xmin
lat <- GIS_pepper_sub$Ymax

#-------------------------------------------
## 1.2 Customize crop and values of parameters
beta0<-0.5                                       ###
beta<-1                                          ###
beta1<-1.5                                       ###
gamma00<-0.05                                    ###
gamma0<-0.1                                      ###
gamma<-0.2                                       ###
gamma1<-0.3                                      ###
gamma2<-1                                        ###
crop<-'Tanoek'
cutoff1<- 0.001  #cropland density cutoff        ###
cutoff2 <- 0.01
cutoff3 <- 0.1
cutoffadja <- 0.01
cutoffadja1 <- 0.001 # cutoff of adjancecy matrix     ###
cutoffadja2 <- 0.01
cutoffadja3 <- 0.1

#  Surveillance analysis: smartsurv analysis
#
# smartsurv is another evaluating the value of nodes for surveillance in studying
# the invasion of a bioentity as part of a smart surveillance strategy.
#
# The stoch = TRUE is selected, then the entries in the matrix are taken as
# probabilities and within a realization an adjacency matrix of 1s and 0s is
# generated based on these probabilities.  The number of realizations for analysis
# was nrealz = 10.
#
# Note that it is assumed that the diagonal of the adjacency matrix is 1 (i.e.,
# that once the bioentity reaches a location it stays there) – otherwise, the
# stopping algorithm will not function correctly.
#
# ## smartsurv
# Perforem smartsurv analysis for the observed Kenya local seed potato trade
# network.

library(devtools)
devtools::install_github("GarrettLab/INA")
library(INA)
# CCRI calculated by Inverse power-law function
#CCRI BY Inverse power-law function
#This version is revised on 07/23/2020

CCRI_smarSurv_powerlaw_function <- function(beta, cutoffadja, distance_matrix, cropValue)   {
    ##############################################
    #### create adjacency matrix

    distancematr <- distance_matrix # pairwise distance matrix
  #---- end of code
    distancematrexp <- distancematr^(-beta) #use function C=AX^(-beta), here A=1, X=distancematr
    cropmatr <- cropValue # complete gravity model with crop data
    cropmatr1 <- matrix(cropmatr, , 1 )
    cropmatr2 <- matrix(cropmatr, 1, )

    cropmatrix <- cropmatr1 %*% cropmatr2
    cropmatrix <- as.matrix(cropmatrix)

    #-------------------------------------------
    # Rescale cropmatrix and distancematrexp to give them the same weight in the gravity model
    cropmatrix <- cropmatrix/max(cropmatrix)
    diag(distancematrexp) <- 0
    distancematrexp <- distancematrexp/ max(distancematrexp)
    #-------------------------------------------
    cropdistancematr <- distancematrexp * cropmatrix # adjacecy matrix

    is.matrix(distancematrexp)
    logicalmatr <- cropdistancematr > cutoffadja # adjacency matrix after threshold
    stan <- cropdistancematr * logicalmatr
    stan <- round(stan, 6) # use round() because betweenness() may have problem when do the calculation
    cropdistancematrix <- graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
    #----------------------------------------------
    #------------------------ smartsurv ------------------------
    # 20211109 add smartsurv
    stan_smart <- stan
    diag(stan_smart) <- 1
    A1 <- smartsurv(adjmat=stan_smart, stoch=T, nrealz=5)
    A1mean <- A1$meanarr
    smartsurv_score_meanarr <- colMeans(A1mean)
    #range(smartsurv_score_meanarr)

    ##############################################
   ## sum of nearest neighbors degree
   knnpref0<-graph.knn(cropdistancematrix,weights=NA)$knn
   knnpref0[is.na(knnpref0)]<-0
   degreematr<-degree(cropdistancematrix)
   knnpref<-knnpref0*degreematr
   if(max(knnpref)==0){knnprefp=0}else
       if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}

   ##############################################
   #### node degree, node strengh
   ####
   nodestrength<-graph.strength(cropdistancematrix)
   nodestrength[is.na(nodestrength)]<-0
     if(max(nodestrength)==0){nodestr=0}else
        if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/6}
   ##############################################
   #### betweenness centrality
   ####
   between<-betweenness(cropdistancematrix)
   between[is.na(between)]<-0
    if(max(between)==0){betweenp=0}else
        if(max(between)>0){betweenp=between/max(between)/2}
   ##############################################
   #### eigenvector and eigenvalues
   ####
   eigenvectorvalues<-evcent(cropdistancematrix)
   ev<-eigenvectorvalues$vector
    ev[is.na(ev)]<-0
    if(max(ev)==0){evp=0}else
        if(max(ev)!=0){evp=ev/max(ev)/6}
   ##############################################
   #### CCRI is a weighted mean of 4 network metric
   ####
 index<-knnprefp+evp+betweenp+nodestr

#    indexpre<-cropRaster
# #   indexpre[]<- NaN
#    indexpre[CellNumber]<- index
#    indexv<-indexpre
#    return(indexv)

   return(list(index,smartsurv_score_meanarr))
   }


# CCRI calculated by negative exponential function

CCRI_smarSurv_negExponential_function <-function(gamma,cutoffadja, distance_matrix, cropValue)   {
    ##############################################
    #### create adjacency matrix
    ####
    distancematr <- distance_matrix


    eulernumber <- exp(1)
    distancematrexponential <- eulernumber ^ (-gamma * distancematr)# exponential model
    cropmatr <- cropValue # complete gravity model with crop data
    cropmatr1 <- matrix(cropmatr,,1) # complete gravity model with crop data
    cropmatr2 <- matrix(cropmatr,1,)
    cropmatrix <- cropmatr1 %*% cropmatr2
    cropmatrix <- as.matrix(cropmatrix)
        #-------------------------------------------
    # Rescale cropmatrix and distancematrexp to give them the same weight in the gravity model
    cropmatrix <- cropmatrix/max(cropmatrix)
    diag(distancematrexponential) <- 0
    distancematrexponential <- distancematrexponential/ max(distancematrexponential)
    #-------------------------------------------

    cropdistancematr <- distancematrexponential * cropmatrix
    logicalmatr <- cropdistancematr > cutoffadja
    stan <- cropdistancematr * logicalmatr
    stan <- round(stan, 6) # use round() because betweenness() may have problem when do the calculation
    cropdistancematrix<-graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
    #------------------------ smartsurv ------------------------
    # 20211109 add smartsurv
    stan_smart <- stan
    diag(stan_smart) <- 1
    A1 <- smartsurv(adjmat=stan_smart, stoch=T, nrealz=5)
    A1mean <- A1$meanarr
    smartsurv_score_meanarr <- colMeans(A1mean)
    #range(smartsurv_score_meanarr)

    ##############################################
    #### create network for all the selected nodes
    ####
    #V(cropdistancematrix)$color=colororder
    V(cropdistancematrix)$label.cex=0.7
    edgeweight<-E(cropdistancematrix)$weight*4000
    E(cropdistancematrix)$color="red"
    #plot(cropdistancematrix,vertex.size=povalue*300,edge.arrow.size=0.2,edge.width=edgeweight,vertex.label=NA,main=paste(crop, sphere1, 'adjacency matrix threshold>',cutoffadja, ', beta=',beta)) # network with weighted node sizes
   # plot(cropdistancematrix,vertex.size=5,edge.arrow.size=0.2,edge.width=edgeweight,vertex.label=NA,main=paste(crop, sphere1, 'adjacency matrix threshold>',cutoffadja, ', beta=',beta)) # network with identical node size
   knnpref0<-graph.knn(cropdistancematrix,weights=NA)$knn
   knnpref0[is.na(knnpref0)]<-0
   degreematr<-degree(cropdistancematrix)
   knnpref<-knnpref0*degreematr
   if(max(knnpref)==0){knnprefp=0}else
       if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}

   ##############################################
   #### node degree, node strengh
   ####
   nodestrength<-graph.strength(cropdistancematrix)
   nodestrength[is.na(nodestrength)]<-0
     if(max(nodestrength)==0){nodestr=0}else
        if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/6}
   ##############################################
   #### betweenness centrality
   ####
   between<-betweenness(cropdistancematrix)
   between[is.na(between)]<-0
    if(max(between)==0){betweenp=0}else
        if(max(between)>0){betweenp=between/max(between)/2}
   ##############################################
   #### eigenvector and eigenvalues
   ####
   eigenvectorvalues<-evcent(cropdistancematrix)
   ev<-eigenvectorvalues$vector
    ev[is.na(ev)]<-0
    if(max(ev)==0){evp=0}else
        if(max(ev)!=0){evp=ev/max(ev)/6}
   ##############################################
   #### plot index layer
   ####
 index<-knnprefp+evp+betweenp+nodestr
# #   indexpre[]<- NaN
#    indexpre<-cropRaster
#    indexpre[CellNumber] <- index
#    indexv<-indexpre
#    return(indexv)
  return(list(index,smartsurv_score_meanarr))

   }



## sensitivity analysis CCRI BY Inverse power-law function and negative exponential

index1 <- CCRI_smarSurv_powerlaw_function(beta = 0.5, cutoffadja, distance_matrix, cropValue )
index2 <- CCRI_smarSurv_powerlaw_function(beta = 1, cutoffadja, distance_matrix, cropValue)
index3 <- CCRI_smarSurv_powerlaw_function(beta = 1.5, cutoffadja, distance_matrix, cropValue)



index4 <- CCRI_smarSurv_negExponential_function(gamma = 0.05, cutoffadja, distance_matrix, cropValue)
index5 <- CCRI_smarSurv_negExponential_function(gamma = 0.1, cutoffadja, distance_matrix, cropValue)
index6 <- CCRI_smarSurv_negExponential_function(gamma = 0.2, cutoffadja, distance_matrix, cropValue)
index7 <- CCRI_smarSurv_negExponential_function(gamma = 0.3, cutoffadja, distance_matrix, cropValue)
index8 <- CCRI_smarSurv_negExponential_function(gamma = 1, cutoffadja, distance_matrix, cropValue)


# Complete sensitivity analysis of CCRI

mean_index <- (index1[[1]]+index2[[1]]+index3[[1]]+index4[[1]]+index5[[1]]+index6[[1]]+index7[[1]]+index8[[1]]) / 8
indexpre <- fd.Agg #potato_bdi_rwa= should be a raster
indexpre[cell_id] <- mean_index
mean_index_raster <- indexpre
#writeRaster(mean_index_raster, "CCRI_potato_bdi_rwa_5min_euclidean_distance.tif")
plot(mean_index_raster)

# Complete sensitivity analysis of smartsurv score

smartsurvScore_index <- (index1[[2]] +index2[[2]] +index3[[2]] +index4[[2]] +index5[[2]] +index6[[2]] +index7[[2]] +index8[[2]]) / 8
indexpre_score <- fd.Agg #potato_bdi_rwa
indexpre_score[cell_id] <- smartsurvScore_index
smartsurv_score_raster <- indexpre_score
#writeRaster(smartsurv_score_raster, "smartsurv_score_potato_bdi_rwa_5min_euclidean_distance.tif")
plot(smartsurv_score_raster)
