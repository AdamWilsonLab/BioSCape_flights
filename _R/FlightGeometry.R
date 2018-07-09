library(sf)

wproj="+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # working projection 

# AVIRIS-NG has a 34 degree FOV so at 65Kft the swath is about 11km, if I recall.  Doing the geometry calcs should confirm.
# AVIRIS-NG spatial sampling is 1 milliradian with an IFOV of about 1.2 milliradians.  So, the ground resolution would be approximately 1/1000th of the distance above ground.  So, 65Kft means a 65 foot GSD.

h=21 #65000*0.3/1000  #altitude in km
fov=34

sw=tan(fov*pi/180)*h  #swath width
sw

ss=1/1000  # spatial sampling in radians
sr=tan(ss)*h*1000  #spatial resolution in m
sr

# from Michael Eastwood, who is the AVIRIS Sensor Team Leader, regarding AVIRIS-NG’s FOV and spatial resolution.

# From the ER-2 Handbook: “A normal 6.5 hour mission will cover a range of 2,400 nm (4,430 km). This will provide for approximately 5.5 hour of data collection at altitudes above 60,000 ft (18 km). An 8 hour mission will cover a distance of 3,000 nm (5,530 km) and produce 7 hour of collection time at altitude.”

d=4430/6.5*5.5 # 6.5 hour flight coverage in km (minus one hour for takeoff/landing)
d


makeFlight=function(origin=c(18.5,-34),angle=0,x=14,y=3750,scale=1,wgs84=T,projection=wproj){
  # assumes origin is in lat/lon, angle in degrees, length and width in km.
  
  # make sf object for origin
  d = st_sf(id=1, geom= st_sfc(st_point(origin)))
  st_crs(d) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  d2=st_transform(d,projection) #warp to working projection
  # create flight polygon
  pgn=list(rbind(c(st_coordinates(d2)[1],st_coordinates(d2)[2]),
             c(st_coordinates(d2)[1]+x*1000,st_coordinates(d2)[2]),
             c(st_coordinates(d2)[1]+(x*1000),st_coordinates(d2)[2]+(y*1000)),
             c(st_coordinates(d2)[1],st_coordinates(d2)[2]+(y*1000)),
             c(st_coordinates(d2)[1],st_coordinates(d2)[2])))%>%
      st_polygon()#%>%
#      st_sfc(crs = projection)%>%
#      st_sf()
    cntrd = st_centroid(pgn)
    angle_radians=angle * pi/180 
    rotation_matrix= matrix(c(cos(angle_radians), sin(angle_radians), 
                              -sin(angle_radians), cos(angle_radians)), 
                            nrow = 2, ncol = 2)
# Rotate it around centroid and scale if desired
# following https://r-spatial.github.io/sf/articles/sf3.html
     pgn2 = ((pgn - cntrd) * rotation_matrix*scale+cntrd) %>%
     st_geometry()%>%st_sf()%>%
      st_set_crs(value=projection)
  if(wgs84) pgn2=st_transform(pgn2,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  return(pgn2)
}

# get map data
cfr_bb=st_bbox(st_buffer(cfr,dist=0.3)); names(cfr_bb)=c("left", "bottom", "right", "top")
gcfrmap <- get_stamenmap(cfr_bb, zoom = 7, maptype = "terrain-background")%>%ggmap()


# Assume 5 hours of data in 10 30min strips
f1=makeFlight(x=3750/10,y=14*10,angle=-45);plot(f1)


#gcfrmap+
#  geom_sf(data=f1,alpha=.5, inherit.aes = FALSE)+
#  ylab("Latitude")+
#  xlab("Longitude")#+  theme_set(theme_bw(25))


