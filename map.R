## Process ecosystem data for web map

library(rgdal)
library(leaflet)
library(rgeos)
library(htmltools)
library(htmlwidgets)
library(raster)
library(dplyr)

eco=readOGR("data/vegm2006_biomes_withforests/vegm2006_biomes_withforests.shp")
eco=subset(eco,BIOMENAME%in%c("Fynbos","Succulent Karoo"))
eco$BIOMENAME=as.character(eco$BIOMENAME)
ecos=gUnionCascaded(eco,id=eco$BIOMENAME)%>%
  gSimplify(0.001,topologyPreserve = T)
ecosd=SpatialPolygonsDataFrame(ecos,data.frame(name=names(ecos),row.names = names(ecos)))

mar=readOGR("data/NBA_marine_benthiccoastal_habitattypes/NBA_benthic_and_coastal_habitat.shp")
projection(mar)=projection(eco)
mars=gUnionCascaded(mar,id=mar$Biogeogr)%>%
  gSimplify(0.001,topologyPreserve = T)
marsd=SpatialPolygonsDataFrame(mars,data.frame(name=names(mars),row.names = names(mars)))

biomes=rbind.SpatialPolygonsDataFrame(ecosd,marsd,makeUniqueIDs = T)


lb=leaflet(biomes)%>% 
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')%>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~colorFactor("Paired",name)(name),
    popup=~htmlEscape(name))


saveWidget(lb, file="map.html")
