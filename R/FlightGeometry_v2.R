library(sf)
library(units)
library(tidyverse)
library(renpow) #for the reference spectra
library(cowplot)
library(renpow)
library(gridExtra)
library(ggforce)
library(ggmap)
library(data.table)
library(ggforce)

wproj="+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # working projection 

# Aircraft
g3_altitude=set_units(c(28000,41000,45000),feet)
g5_altitude=set_units(c(28000,41000,51000),feet)
h_feet=g3_altitude

h=set_units(h_feet, km)  #altitude in km
h

# AVIRIS-NG 
# https://avirisng.jpl.nasa.gov/specifications.html
AVIRISNG=data.frame(
  instrument="AVIRIS-NG",
  row=1, #just used for plotting
  tfov=set_units(36,degrees),
  ifov=set_units(1,mrad),
  spec_n=480,
  spec_res=set_units(5,nm),
  spec_min=set_units(380,nm),
  spec_max=set_units(2510,nm)
)


#### HyTES https://directory.eoportal.org/web/eoportal/airborne-sensors/hytes
HyTES=data.frame(
  instrument="HyTES", 
  row=2, #just used for plotting
  tfov=set_units(50,degrees),
  ifov=set_units(1.44,mrad),
  spec_n=256,
  spec_res=set_units(17.6,nm),
  spec_min=set_units(set_units(7.5,um),nm),
  spec_max=set_units(set_units(12,um),nm)
)


#### PRISM (https://prism.jpl.nasa.gov/spectrometer_char.html)
PRISM=data.frame(
  instrument="PRISM",
  row=3, #just used for plotting
  tfov=set_units(30.7,degrees),
  ifov=set_units(0.97,mrad),
  spec_n=608,
  spec_res=set_units(2.83,nm),
  spec_min=set_units(349.9,nm),
  spec_max=set_units(1053.5,nm)
)


#### LVIS
LVIS=data.frame(
  instrument="LVIS",
  row=4, #just used for plotting
  tfov=set_units(200,mrad) %>% set_units(degrees), #https://lvis.gsfc.nasa.gov/Data/Maps/ABoVE2019Map.html
  ifov=set_units(0.75,mrad),
  spec_n=1,
  spec_res=set_units(1,nm),
  spec_min=set_units(1064,nm),
  spec_max=set_units(1064,nm)
)


sensors=bind_rows(AVIRISNG,HyTES,PRISM,LVIS) %>% 
  mutate(
    swath_width_low=tan(tfov)*min(h),  #swath width low altitude
    swath_width_mid=tan(tfov)*median(h),  #swath width low altitude
    swath_width_high=tan(tfov)*max(h),  #swath width high altitude
    spatial_resolution_low=tan(set_units(ifov,radians))*min(h) %>% set_units(m),  # spatial resolution in m
    spatial_resolution_mid=tan(set_units(ifov,radians))*median(h) %>% set_units(m),  # spatial resolution in m
    spatial_resolution_high=tan(set_units(ifov,radians))*max(h) %>% set_units(m)) %>%   # spatial resolution in m
  mutate(
    swath_width_km=paste0("[",round(swath_width_low,2),",",
                          round(swath_width_high,2),"]"),
    spatial_resolution_m=paste0("[",round(spatial_resolution_low,2),",",
                                round(spatial_resolution_high),"]"))

data.table::DT(sensors)

sensors %>% 
  select(instrument,
         spectral_bands=spec_n,spectral_resolution=spec_res,
         swath_width_km,spatial_resolution_m) %>% 
DT::datatable()


#################
## Import other sensor data
sats=read_csv("data/rs_sensors.csv") %>% 
  mutate(spec_min=set_units(wavelength_start,nm),
         spec_max=set_units(wavelength_stop,nm),
         spatial_resolution=set_units(resolution,m),
         row=as.numeric(factor(Sensor)))

solar=ASTMG173 %>% 
  mutate(wv=set_units(Wvlgth.nm,nm),
         irradiance=set_units(Direct.circumsolar.W.m.2.nm.1,watts/m^2)) %>% as_tibble()


p1=ggplot(solar,aes(x=wv,y=irradiance))+
  geom_line()+
  ylab("Solar_Irradiance")+
  xlab("Wavelength")


p2=ggplot()+geom_segment(aes(y=instrument,yend=instrument,x=spec_min,xend=spec_max),
            data=sensors,inherit.aes = F,size=20)
p3=ggplot()+geom_segment(aes(y=Sensor,yend=Sensor,x=spec_min,xend=spec_max),
          data=sats,inherit.aes = F,size=20,alpha=.5)

grid.arrange(p1, p2, p3)
plot_grid(p1, p2,p3, align = "v",ncol=1)

ggplot(sensors)+
  geom_rect(aes(ymin=row-0.25,ymax=row+0.25,xmin=spec_min,xmax=spec_max),
            data=sensors,inherit.aes = F)


# G3 https://jsc-aircraft-ops.jsc.nasa.gov/gulfstream-giii.html
# 6 hours / 2,600 nautical miles = 4815.2 km
d=4815*(5/6) # 6 hour flight coverage in km (minus one hour for takeoff/landing)
d
swath_length=40

p_jonkershoek=c(18.995,-34.1027)
p_capepoint=c(18.435923,-34.359561)
p_capepoint=c(18.4232,-34.2441)

makeFlight=function(origin=c(18.995,-34.1027),angle=0,swath_width,swath_length,scale=1,wgs84=F,projection=wproj){
  # assumes origin is in lat/lon, angle in degrees, length and width in km.
  if(units(swath_width)$numerator!="km") error("swath_width must have units that are convertable to km")
  # make sf object for origin
  d = st_sf(id=1, geom= st_sfc(st_point(origin)))
  st_crs(d) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  d2=st_transform(d,projection) #warp to working projection
  # create flight polygon
  pgn=list(rbind(c(set_units(st_coordinates(d2)[1],m),set_units(st_coordinates(d2)[2],m)),
             c(set_units(st_coordinates(d2)[1],m)+set_units(swath_width,m)/2,set_units(st_coordinates(d2)[2],m)),
             c(set_units(st_coordinates(d2)[1],m)+set_units(swath_width,m)/2,set_units(st_coordinates(d2)[2],m)+set_units(swath_length,m)),
             c(set_units(st_coordinates(d2)[1],m)-set_units(swath_width,m)/2,set_units(st_coordinates(d2)[2],m)+set_units(swath_length,m)),
             c(set_units(st_coordinates(d2)[1],m)-set_units(swath_width,m)/2,set_units(st_coordinates(d2)[2],m)),
             c(set_units(st_coordinates(d2)[1],m),set_units(st_coordinates(d2)[2],m))))%>%
      st_polygon()
    cntrd = st_centroid(pgn)
    angle_radians=angle * pi/180 
    rotation_matrix= matrix(c(cos(angle_radians), sin(angle_radians), 
                              -sin(angle_radians), cos(angle_radians)), 
                            nrow = 2, ncol = 2)
# Rotate it around centroid and scale if desired
# following https://r-spatial.github.io/sf/articles/sf3.html
     pgn2 = ((pgn - cntrd) * rotation_matrix*scale+cntrd) %>%
     st_geometry()%>%#st_sf()%>%
      st_set_crs(value=projection)
  if(wgs84) pgn2=st_transform(pgn2,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  return(pgn2)
}


# make example path
path=st_linestring(x=as.matrix(rbind(c(18.995,-34.1027),c(21.4232,-34.2441))),dim="XY") %>% 
  st_sfc() %>% st_sf(path=1,geom=.,crs=4326)


make_flight_line=function(path,swath_width,wgs84=F,segment_size=1000){
  # assumes origin/destination is in lat/lon, length and width in km.
  if(!"sf" %in% class(path)) stop("path must be an sf object")
  if(units(swath_width)$numerator!="km") stop("swath_width must have units that are convertable to km")
  # make sf object for origin
  path2=path %>% #st_transform(projection) %>%  #warp to working projection=
      st_segmentize(line, dfMaxLength = segment_size) #segmentize (adds more points to lines)

    coords=st_coordinates(path2) %>%
      as_tibble() %>% 
      select(-L1)
    coords$bearing=geosphere::bearing(coords)
    coords[,c("x_left","y_left")]=destPoint(coords,b=coords$bearing-90,d=1000*set_units(swath_width,"km")/2)
    coords[,c("x_right","y_right")]=destPoint(coords,b=coords$bearing+90,d=1000*set_units(swath_width,"km")/2)
    coords=na.omit(coords)
    
    pgn=list(rbind(cbind(coords$x_left,coords$y_left),
                   cbind(rev(coords$x_right),rev(coords$y_right)),
                   cbind(coords$x_left,coords$y_left)[1,])) %>% 
      st_polygon() %>% 
      st_geometry()%>%#st_sf()%>%
      st_set_crs(value=projection)
    if(wgs84) pgn=st_transform(pgn,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  return(pgn)
}

make_flight_line(path=path,swath_width = set_units(10,"km"))

flights <- sensors %>% 
  rowwise() %>% 
  mutate(geometry=make_flight_line(path=path,
                                   swath_width = swath_width_mid)) %>% 
  st_as_sf() %>% 
  mutate(instrument=factor(instrument,ordered=T,levels = c("HyTES","AVIRIS-NG","PRISM","LVIS")))


flight_line <-
  makeFlight(origin = p_capepoint,
                             swath_width = set_units(0.01,km),
                             swath_length=set_units(swath_length,km)) %>% 
  st_as_sf()


bbox=st_transform(flights,4326) %>% 
  st_buffer(dist = 0.05) %>% 
  st_bbox();names(bbox)=c("left","bottom","right","top")

hdf <- get_map(location=bbox,
              maptype="toner", zoom=12,
              source="stamen")
hdf <- get_map(location=bbox,
               maptype="satellite", zoom=12)


ggmap(hdf, extent = "normal")+
  geom_sf(data=st_transform(flights,4326),mapping=aes(fill=instrument),
          alpha=.4,color=NA,inherit.aes = F)+
  ylab("Latitude")+
  xlab("Longitude")


flights2 <- flights %>% 
  st_segmentize(1)

ggplot(flights,aes(fill=instrument),alpha=.5,color=NA)+
  geom_sf()+
  coord_sf(crs=wproj)


# write KML for Google Earth Visualization
flights %>% 
  st_segmentize(50) %>% 
#  mutate(OGR_STYLE = "BRUSH(fc:#0000FF80); PEN(c:#FF0000,w:1px)") %>%  
  st_write("data/transect.kml",append=F,altitudeMode="clampToGround")#,
#           driver = "libkml")

flights %>% 
  st_union() %>% 
  st_segmentize(50) %>% 
  st_write("data/transect_union.kml",append=F,altitudeMode="clampToGround")



flight_line %>% 
  st_segmentize(30) %>% 
  st_write("data/flight_line.kml",append=F,altitudeMode="clampToGround",
           driver = "libkml")




# get map data
# 
#gcfr=read_sf("data/gcfr.shp")

#cfr_bb=st_bbox(st_buffer(gcfr,dist=0.3)); names(cfr_bb)=c("left", "bottom", "right", "top")
#gcfrmap <- get_stamenmap(cfr_bb, zoom = 7, 
#                         maptype = "terrain-background")%>%ggmap()


# Assume 5 hours of data in 10 30min strips
#f1=makeFlight(origin=c(18,-34), x=3750/10,y=14*10,angle=-45);plot(f1)


#gcfrmap+
#  geom_sf(data=f1,alpha=.5, inherit.aes = FALSE)+
#  ylab("Latitude")+
#  xlab("Longitude")#+  theme_set(theme_bw(25))


