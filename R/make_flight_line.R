make_flight_line=function(path,swath_width,wgs84=F,segment_size=1000,projection=wproj){
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
      st_set_crs(value=4326) %>% 
      st_transform(projection) %>% 
      st_buffer(dist=0) %>% 
      st_make_valid() %>% 
      st_transform(4326)
  return(pgn)
}
