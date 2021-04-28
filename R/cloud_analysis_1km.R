# Cloud analysis
# 
library(raster)
library(rasterVis)
library(ggplot2)
library(sf)
library(ggpubr)
library(spatialEco)
library(ggridges)
library(tibble)
library(ggpmisc)
library(doParallel)
registerDoParallel()

# domain
gcfr=read_sf("data/gcfr.shp") %>% st_simplify(dTolerance = 0.01)

##########################

## MODIS 1km from earth engine

mod=brick("~/Documents/ee_mcd09cf/20200529_DailyCF_MOD09GA.tif")[[-1]]
projection(mod)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
names(mod)=1:365



## smooth version
cp=raster(extent(18.3,18.5,-34.4,-34),res=0.008,vals=1)
projection(cp)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#cp_sin <- projectRaster(cp,crs=mod,method = "ngb",alignOnly=T)
mod2=crop(mod,projectExtent(cp,mod))

impute.loess<-function(y,s, ...){
  x <- 1:length(y)
  p <- stats::loess(y ~ x, span = 0.2, 
                    data.frame(x = x, 
                      y = y, na.rm=T))
    y <- stats::predict(p, x)
    return(y)
}

#beginCluster(4)

rasterOptions(progress="text")

mods<-calc(mod, fun=impute.loess, unstack = TRUE, forcefun = FALSE, na.rm=T,
               filename="data/cloud/mod09GA_smooth.tif",
               overwrite=T)

# mods <- clusterR(mod2, verbose=T, 
#                  fun=calc, args=list(fun=impute.loess), 
#                  filename="data/cloud/mod09GA_smooth.tif",
#                  overwrite=T)
# 
i=1

# reproject roi
gcfr_s <- st_transform(gcfr,st_crs(mods))



foreach(i=1:nlayers(mod),.packages = c("rasterVis","ggplot2")) %do% {
print(i)
tdate=format(as.Date(as.character(i),format="%j"),"%B %d")

# histogram
hd = density(values(mods[[i]])/100, n = 1000, bw=4) 
  
h1=ggplot(data.frame(x = hd$x, y = hd$y), aes(x, y))+
  geom_segment(aes(xend = x, yend = 0, colour = x)) + 
  scale_color_viridis_c()+
  xlim(0,100)+
#  ylim(0,0.05)+
  ylab("")+
  xlab("Mean Cloud Fraction (% Cloudy Days per month)\n MODIS Terra and Aqua")+
  theme(legend.position = "none")

#inset <- tibble(x = 15, y = -32, plot = list(h1))

m1 = mods[[i]] %>% 
  gplot(maxpixels=1e6)+
  geom_raster(aes(fill=value/100))+
  scale_fill_viridis_c(
    name="Mean Cloud Fraction (% Cloudy Days per month)\n MODIS Terra and Aqua",
    limits=c(0,100))+
  geom_sf(data=gcfr_s,inherit.aes=F,fill=NA,
          size=.25,col="black")+
  coord_sf(label_graticule = "")+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  annotate("text",label=tdate, x = 1276697, y=-3058864,vjust="top",
           hjust="left", size = 8, colour = "white")
#  geom_plot(data = inset, aes(x, y, label = plot))

# write it out to disk  
fig = ggarrange(m1,h1,heights=c(0.8,0.2),ncol=1,align ="v")

ggsave(plot=fig, 
       filename = file.path("output/cloudanim",
                            paste0("modcloud_",
                                   sprintf("%03d", i),".png")),
       width=11,height=8)

return(i)
  
}

system("ffmpeg -y -r 10 -f image2 -s 1920x1080 -i output/cloudanim/modcloud_%03d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p output/cloudanim.mp4")
       

