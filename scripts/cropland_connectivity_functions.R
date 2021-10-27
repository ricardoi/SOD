# ---
#'@title: "Functions: to generate data.frame with GIS info, 
#          and models to generate values of harvest cropland fraction"
#'@author: "Y Xing"
#'@date: "08/20/2021"
#'@output: Cropland Models

# load libraries
library(geosphere)
#-----------------------------------------------------
#  4. Function to generate data frame with GIS info, 
#     and values of harvest cropland fraction
#-----------------------------------------------------

FuncGIScroplandW <- function(cropRaster, HCFcutoff){
  cropRasterValues <- getValues(cropRaster)
  cellNum <- which(cropRasterValues > HCFcutoff)
  cellNumValue <- cropRasterValues[cellNum]
  length <- length(cellNumValue)
  colnums <- ncol(cropRaster)
  lati <- c(rep(0, length))
  logi <- c(rep(0, length))
  for (k in c(1:length)) {
    me<-(cellNum[k]-1)%/%colnums
    lati[k]<-latito-me*CellDegree
    logi[k]<-(cellNum[k]-1-me*colnums)*CellDegree+longifrom
  }
  cropdata <- cbind(lati, logi, cellNumValue)
  return(as.data.frame( cropdata))
}

#-----------------------------------------------------
#  5. Inverse Power Law Model
#-----------------------------------------------------
networkbeta <- function(cropdata, beta3, cutoffadja3){
  # create adjacency matrix
  rownumber13 <- nrow(cropdata)
  latilongimatr3 <- cropdata[1:rownumber13,c(2,1)] # save the latitude and longitude as new matrix
  # Package geosphere::distVincentyEllipsoid() used to calculate the distance <defult distance is meter>
  dvse <- geosphere::distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree
  
  latilongimatr3 = as.matrix(latilongimatr3)
  TemMat <- matrix(NA, nrow(latilongimatr3), nrow(latilongimatr3))
  
  for (i in 1:nrow(latilongimatr3)){
    TemMat[i, ] <- geosphere::distVincentyEllipsoid(latilongimatr3[i,], latilongimatr3)/dvse
  }
  
  distancematr3 = TemMat
  #---- end of input code
  ############################################
  #### beginning Inverse Powerlaw Model
  distancematrexp3 <- distancematr3^(-beta3)  # use function C = AX^(-beta), here A = 1, X = distancematr3
  cropmatr3 <- cropdataW[1:rownumber13, 3] # complete gravity model with crop data
  cropmatr13 <- matrix(cropmatr3,,1)
  cropmatr23 <- matrix(cropmatr3,1,)
  cropmatrix3 <- cropmatr13 %*% cropmatr23
  cropmatrix3 <- as.matrix(cropmatrix3)
  cropdistancematr3 <- distancematrexp3 * cropmatrix3
  logicalmatr3 <- cropdistancematr3 > cutoffadja3
  stan3 <- cropdistancematr3 * logicalmatr3
  cropdistancematrix3 <- graph.adjacency(stan3, mode = c("undirected"), diag = FALSE, weighted = TRUE) #change the thresh to see the difference
  ##############################################
  #### create network for all the selected nodes
  ####
  V(cropdistancematrix3)$label.cex = 0.7
  E(cropdistancematrix3)$color = "red"
  edgeweight3 <- E(cropdistancematrix3)$weight * 10000
  knnpref0 <- graph.knn(cropdistancematrix3, weights = NA)$knn
  knnpref0[is.na(knnpref0)] <- 0
  degreematr <- degree(cropdistancematrix3)
  knnpref <- knnpref0 * degreematr
  if(max(knnpref) == 0){knnprefp = 0} else
    if(max(knnpref) > 0){knnprefp = knnpref / max(knnpref) / 6}
  
  ##############################################
  ####  node degree, node strengh
  ####
  
  nodestrength <- graph.strength(cropdistancematrix3)
  nodestrength[is.na(nodestrength)] <- 0
  if(max(nodestrength) == 0){nodestr = 0} else
    if(max(nodestrength) > 0){nodestr = nodestrength / max(nodestrength) / 6}
  ##############################################
  ####  betweenness centrality
  ####
  between <- betweenness(cropdistancematrix3)
  between[is.na(between)] <- 0
  if(max(between) == 0){betweenp = 0} else
    if(max(between) > 0){betweenp = between / max(between) / 2}
  ##############################################
  ####   eigenvector and eigenvalues
  ####
  eigenvectorvalues <- evcent(cropdistancematrix3)
  ev <- eigenvectorvalues$vector
  ev [is.na(ev)] <-0
  if(max(ev) == 0){evp = 0} else
    if(max(ev) != 0){evp = ev / max(ev) / 6}
  ##############################################
  ####   plot index layer
  ####
  index <- knnprefp + evp + betweenp + nodestr
  indexpre <- cropharvestRasterWaggValues
  indexpre[] <- 0
  indexpre [cellNumW] <- index
  indexv3 <- indexpre
  return(indexv3)
}
#-----------------------------------------------------
#  6. Negative Exponential Model
#-----------------------------------------------------
networkgamma <- function(cropdata, gamma3, cutoffadja3){
  ##############################################
  #### create adjacency matrix
  ####
  rownumber13 <- nrow(cropdata)
  latilongimatr3 <- cropdata[1:rownumber13,c(2,1)]# save the latitude and longitude as new matrix  
  # geosphere::distVincentyEllipsoid() is used to calculate the distance, default distance is meter
  dvse <- geosphere::distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree
  
  latilongimatr3 <- as.matrix(latilongimatr3)
  TemMat <- matrix(-999, nrow( latilongimatr3),nrow(latilongimatr3))
  
  for (i in 1:nrow(latilongimatr3)) {
    TemMat[i, ] <- geosphere::distVincentyEllipsoid(latilongimatr3[i,], latilongimatr3) / dvse
  }
  
  distancematr3 <- TemMat
  #---- end of input code
  ############################################
  #### beginning Negative Exponential Model 
  eulernumber <- exp(1)
  distancematrexponential3 <- eulernumber^(-gamma3 * distancematr3) # neg. exponential model
  
  cropmatr3 <- cropdata[1:rownumber13,3] # complete gravity model with crop data
  cropmatr13 <- matrix(cropmatr3,,1)
  cropmatr23 <- matrix(cropmatr3,1,)
  cropmatrix3 <- cropmatr13 %*% cropmatr23
  cropmatrix3 <- as.matrix(cropmatrix3)
  cropdistancematr3 <- distancematrexponential3 * cropmatrix3
  logicalmatr3 <- cropdistancematr3 > cutoffadja3
  stan3 <- cropdistancematr3 * logicalmatr3
  cropdistancematrix3 <- graph.adjacency(stan3, mode = c("undirected"),
                                       diag = FALSE, weighted = TRUE) #change the threshold to see the difference
  ##############################################
  #### create network for all the selected nodes
  ####
  V(cropdistancematrix3)$label.cex = 0.7
  E(cropdistancematrix3)$color = "red"
  edgeweight3 <- E(cropdistancematrix3)$weight * 10000
  knnpref0 <- graph.knn(cropdistancematrix3, weights=NA)$knn
  knnpref0[is.na(knnpref0)] <- 0
  degreematr <- degree(cropdistancematrix3)
  knnpref <-knnpref0 * degreematr
  if(max(knnpref) == 0){knnprefp = 0} else
    if(max(knnpref) > 0){knnprefp = knnpref / max(knnpref) / 6}
  ##############################################
  ####  node degree, node strengh
  ####
  nodestrength <- graph.strength(cropdistancematrix3)
  nodestrength[is.na(nodestrength)] <- 0
  if(max(nodestrength) == 0){nodestr = 0} else
    if(max(nodestrength) > 0){nodestr = nodestrength / max(nodestrength) / 6}
  ##############################################
  ####  betweenness centrality
  ####
  between <- betweenness(cropdistancematrix3)
  between[is.na(between)] <- 0
  if(max(between) == 0){betweenp = 0} else
    if(max(between) > 0){betweenp = between / max(between) / 2}
  ##############################################
  ####   eigenvector and eigenvalues
  ####
  eigenvectorvalues <- evcent(cropdistancematrix3)
  ev <- eigenvectorvalues$vector
  ev[is.na(ev)] <- 0
  if(max(ev) == 0){evp = 0} else
    if(max(ev) != 0){evp = ev / max(ev) / 6}
  ##############################################
  ####   plot index layer
  ####
  index<- knnprefp + evp + betweenp + nodestr
  indexpre <- cropharvestRasterAggValues
  indexpre[]<-0
  indexpre[cellNumW]<-index
  indexv3<-indexpre
  return(indexv3)
  
}
#-----------------------------------------------------