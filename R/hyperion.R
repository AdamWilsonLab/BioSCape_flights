# Cape point Hyperion
# 
# 
# 
library(dplyr)
library(raster)
library(sf)
library(tidyr)
library(ggplot2)

d=stack(as.list(list.files("~/Downloads/hyperion/EO1H1750832002097110KY 2/",pattern="TIF",full=T)))

plot(d[[1]])

## read in plot data
p=read.csv("~/Downloads/hyperion/jslingsby-Slingsby_TaylorPlots-bc5419a/201005_PlotLocations.csv")
p=p[!is.na(p$Lat_map)&!is.na(p$Lat_map),]
coordinates(p)=c("Lon_map","Lat_map")
#p=st_as_sf(p)
projection(p)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
p=spTransform(p,projection(d))

pd=raster::extract(d,p)%>%as.data.frame()
pd$plot=p$PLOT

pdl=gather(pd,key = band,value=refl,-plot)%>%
  separate(band,sep = "_",into = c("scene","band","type"))
pdl$band=sub("B","",pdl$band)%>%as.numeric()

bands=read.csv("~/Downloads/hyperion/Hyperion_Spectral_Coverage.csv")
bands$band=sub("B","",bands$Hyperion.Band)%>%as.numeric()

# Hyperion solar irradiances 
hypsi=read.table("~/Downloads/hyperion/hyp_irradiance.txt",skip=5)
colnames(hypsi)=c("bandn","ir")
hypsi2=hypsi%>%separate(bandn,sep = "_",into = c("band","wv"))%>%
  mutate(band2=as.numeric(sub("b","",band)))

# convert to reflectance
# https://eo1.usgs.gov/faq/question?id=21
dist=1.0076

pdl$l=bands$Average.Wavelength..nm.[match(pdl$band,bands$band)]
pdl$ir=hypsi2$ir[match(pdl$band,hypsi2$band2)]
pdl$r=(pi*pdl$refl*dist^2)/(pdl$ir*cos((90-37.775783)*(pi/180)))
  
  
badbands=c(1:7,58:76,121:126,167:180,222:242)

pdl=filter(pdl,refl>1)
pdl$r[pdl$band%in%badbands]=NA

## import plot data
## 
veg=read.csv("~/Downloads/hyperion/jslingsby-Slingsby_TaylorPlots-bc5419a/veg1996.csv")
traits=read.csv("~/Downloads/hyperion/jslingsby-Slingsby_TaylorPlots-bc5419a/traits.csv")

env=read.csv("~/Downloads/hyperion/jslingsby-Slingsby_TaylorPlots-bc5419a/enviroment.csv")
colnames(env)=tolower(colnames(env))

pdl2=left_join(pdl,env,by="plot")


### make some plots
p1=ggplot(pdl2,aes(y=r,x=l,group=plot, color=moisture_class))+
  geom_line()
p1

p1+ facet_wrap(~plot)

p1+ facet_wrap(~age1996)
