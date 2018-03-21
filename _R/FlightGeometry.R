library(sf)



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

d=5530 # 8 hour flight coverage in km

makeFlight=function(origin=c(-34,18.5),height=5530,width=14){
    paste0("POLYGON((",
         origin[1]," ",origin[2],", ",
         origin[1]," ",origin[2],", ))")

}



