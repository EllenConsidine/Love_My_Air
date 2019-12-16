library(raster)
library(sp)
library(rgeos)
library(rgdal)

#Read in monitor coordinates
LL<- read.csv("~/Collo_LatLon.csv")

#Project sensor coordinates
monitors<- SpatialPoints(LL[4:5,2:3])
proj4string(monitors)<- CRS( "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
Monitors<- spTransform(monitors, CRS( "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# #Read in road shapefile
# Source: https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-street-centerline
# SHP<- shapefile("~/street_centerline.shp")
# SHP_prj<- spTransform(SHP, CRS( "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# #Separate by size
# large<- SHP_prj[which(SHP_prj$VOLCLASS == c("ARTERIAL")),]
# medium<- SHP_prj[which(SHP_prj$VOLCLASS == c("COLLECTOR")),]
# small<- SHP_prj[which(SHP_prj$VOLCLASS == c("LOCAL")),]
# 
# writeOGR(obj = large, layer = "Arterial_roads", "~/DENVER_map", driver = "ESRI Shapefile")
# writeOGR(obj = medium, layer = "Collector_roads", "~/DENVER_map", driver = "ESRI Shapefile")
# writeOGR(obj = small, layer = "Local_roads", "~/DENVER_map", driver = "ESRI Shapefile")

large<- shapefile("~/Arterial_roads.shp")
medium<- shapefile("~/Collector_roads.shp")
small<- shapefile("~/Local_roads.shp")

##Make buffers 
#Note: this projection is in meters
Buffers<- c(50, 100, 250, 500)

#Line for creating test set:
LL<- LL[4:5,]

for(b in 1:length(Buffers)){
  gbuffer<- gBuffer(Monitors, width = Buffers[b], byid = TRUE)
  
  #Sum road lengths which intersect with buffers
  LL<- cbind(LL, over(gbuffer, large[,c("LEN_MI")], fn= sum),
             over(gbuffer, medium[,c("LEN_MI")], fn= sum),
             over(gbuffer, small[,c("LEN_MI")], fn= sum))
}

LL[is.na(LL)]<- 0

names(LL)<- c("Sensor", "Longitude", "Latitude", sapply(Buffers, function(x){c(paste0("Aroad_", as.character(x)),
                                                                     paste0("Croad_", as.character(x)),
                                                                     paste0("Lroad_", as.character(x)))}))

write.csv(LL, "~/Road_lengths_3.csv", row.names = FALSE)
