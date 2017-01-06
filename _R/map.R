## Process ecosystem data for web map

library(rgdal)
library(leaflet)
library(rgeos)
library(htmltools)
library(htmlwidgets)
library(raster)
library(dplyr)

eco=readOGR("_data/vegm2006_biomes_withforests/vegm2006_biomes_withforests.shp")
eco=subset(eco,BIOMENAME%in%c("Fynbos","Succulent Karoo"))
eco$BIOMENAME=as.character(eco$BIOMENAME)
ecos=gUnionCascaded(eco,id=eco$BIOMENAME)%>%
  gSimplify(0.001,topologyPreserve = T)
ecosd=SpatialPolygonsDataFrame(ecos,data.frame(name=names(ecos),row.names = names(ecos)))

mar=readOGR("_data/NBA_marine_benthiccoastal_habitattypes/NBA_benthic_and_coastal_habitat.shp")
projection(mar)=projection(eco)
mar=subset(mar,!Biogeogr%in%c("Southeast Atlantic","South Atlantic","Southwest Indian","Natal","Natal-Delagoa","Delagoa","Harbour"))
mar$Biogeogr=as.character(mar$Biogeogr)
mars=gUnionCascaded(mar,id=mar$Biogeogr)%>%
  gSimplify(0.001,topologyPreserve = T)
marsd=SpatialPolygonsDataFrame(mars,data.frame(name=names(mars),row.names = names(mars)))

biomes=rbind.SpatialPolygonsDataFrame(ecosd,marsd,makeUniqueIDs = T)

#plot(biomes)

lb=leaflet(biomes)%>% 
  addTiles()%>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~colorFactor("Paired",name)(name),
    popup=~htmlEscape(name))


saveWidget(lb, file="map.html")
