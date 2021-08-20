library(sf)
library(rgeos)
library(ggplot2)
#
library("rnaturalearth")
library(rnaturalearthdata)
library("maps")
library("tools")


#---- setting working directory
setwd("/Users/ricardoi/Box/OSU/SOD/shapefiles/")


# Select world data
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# # Getting states lines
# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# states$ID <- toTitleCase(states$ID)
# # Getting county lines
# counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
# counties <- subset(counties, grepl("oregon", counties$ID))[-1,]
# counties$area <- as.numeric(st_area(counties))
# head(counties)

# loading SOD data
#---- HD_98_Sites
HD_98_Sites <- st_read("Pram_HD_eradicationNA1EU1/HD_98_Sites.shp")
sites <- data.frame(longitude=HD_98_Sites$Longitude, 
                    latitude=HD_98_Sites$Latitude)
head(sites)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkred") +
  # geom_sf(data = counties, fill = NA ) +
  coord_sf(xlim = c(-124.5, -124), ylim = c(42, 42.5), expand = T)




#---- HD_98_Sites
HD_98_Sites <- st_read("Pram_HD_eradicationNA1EU1/HD_98_Sites.shp")
st_geometry_type(HD_98_Sites)
st_crs(HD_98_Sites)
st_bbox(HD_98_Sites)
HD_98_Sites[,10:11]


ggplot() + 
  geom_sf(data = HD_98_Sites, size = 3, aes(color = Elevation), fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()



#------ Export_Output.shp

EO <- st_read("Pram_resequencedNA1EU1//Export_Output.shp")
st_geometry_type(EO)
st_crs(EO)
st_bbox(EO)
EO

ggplot() + 
  geom_sf(data = EO, size = 3, aes(color = Year), fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()


#--- Pram_resequencedNA1EU1.shp

Pram_res <- st_read("Pram_resequencedNA1EU1//Pram_resequencedNA1EU1.shp")
st_geometry_type(Pram_res)
st_crs(Pram_res)
st_bbox(Pram_res)
Pram_res

ggplot() + 
  geom_sf(data = Pram_res, size = 1, aes(color = Lin), fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()


