library(sf)
library(ggplot2)

setwd("/Users/ricardoi/Box/OSU/SOD/shapefiles/")


aoi_boundary_HARV <- st_read("Pram_HD_eradicationNA1EU1/HD_98_Sites.shp")
st_geometry_type(aoi_boundary_HARV)
st_crs(aoi_boundary_HARV)
st_bbox(aoi_boundary_HARV)
aoi_boundary_HARV


ggplot() + 
  geom_sf(data = aoi_boundary_HARV, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()
