
library(rgdal)
library(leaflet)
library(rgeos)
library(raster)
library(rasterVis)
library(dplyr)
library(ggplot2)
library(sf)
library(ggmap)

# import dem
dem=raster("_data/dem/10mdem.tif")


cp=readOGR("_data/CapePoint1969VegMap/CP_Vegmap.shp")%>%
  spTransform(CRSobj = projection(dem))
cpf=levels(cp$Veg)

demc=crop(dem,extent(gBuffer(cp,width = 100)))
names(demc)="dem"

cpr=rasterize(cp,demc,field="Veg")
names(cpr)="vegid"

cprm=MoranLocal(cpr)
plot(cprm)

transect=SpatialLinesDataFrame(
#  readWKT("LINESTRING(259430.3 -3795919, 267732 -3795919 )"),
  readWKT("LINESTRING( 261550.8 -3796009 ,   264511.4 -3794878   )"),
  data.frame(Z = c("transect")))
projection(transect)=projection(cp)

trans=extract(x=stack(demc,cpr),
                      y=transect,
                      along=T,
                      cellnumbers=T,
                      small=T)%>%
  data.frame()

trans$veg=cpf[trans$veg]

trans[,c("x","y")]=coordinates(cpr)[trans$cell,]
trans$order=as.integer(rownames(trans))
trans=arrange(trans,order)

gplot(demc)+
  geom_raster(aes(fill=value))+
  geom_line(data=fortify(transect),
            aes(x=long,y=lat),col="red")+
  geom_sf(data=st_as_sf(cp),aes(color=Veg),
          inherit.aes =F, fill=NA)#+
#  xlim()+ylim()

ggplot(trans,aes(x=x,y=dem,color=veg,group=1))+
  geom_step(size=.5)#+
#  xlim(c(259000,260100))
