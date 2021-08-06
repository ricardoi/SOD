library(sf)
library(ggplot2)

setwd("/Users/ricardoi/Box/OSU/SOD/shapefiles/")

#---- HD_98_Sites
HD_98_Sites <- st_read("Pram_HD_eradicationNA1EU1/HD_98_Sites.shp")
st_geometry_type(HD_98_Sites)
st_crs(HD_98_Sites)
st_bbox(HD_98_Sites)
HD_98_Sites


ggplot() + 
  geom_sf(data = HD_98_Sites, size = 3, aes(color = Lineage), fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()

#------ Export_Output.shp

EO <- st_read("Pram_resequencedNA1EU1//Export_Output.shp")
st_geometry_type(EO)
st_crs(EO)
st_bbox(EO)
EO

ggplot() + 
  geom_sf(data = EO, size = 3, aes(color = Lin), fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()


#--- Pram_resequencedNA1EU1.shp

Pram_res <- st_read("Pram_resequencedNA1EU1//Pram_resequencedNA1EU1.shp")
st_geometry_type(Pram_res)
st_crs(Pram_res)
st_bbox(Pram_res)
Pram_res

ggplot() + 
  geom_sf(data = Pram_res, size = 3, aes(color = Li), fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()


