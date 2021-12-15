# ---
#'@title: "Functions: to generate data.frame with GIS info, 
#          and models to generate values of harvest cropland fraction"
#'@author: "Y Xing"
#'@date: "08/20/2021"
#'@output: Cropland Models

# load libraries
library(geosphere)
#install INA

#-----------------------------------------------------
#  4. Function to generate data frame with GIS info, 
#     and values of harvest cropland fraction
#-----------------------------------------------------
#on an adjacency matrix of 1s and 0s is
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

#----------------------------------------------------------
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
