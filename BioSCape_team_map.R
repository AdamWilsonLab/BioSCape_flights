# Load required packages
library(googlesheets4)
library(tidyverse)
library(leaflet)
library(ggmap)
library(sf)
library(geosphere)
# Authenticate with Google Drive API
gs4_auth()

# Define Google Sheet ID and worksheet name
sheet_id <- "1b2noqwSYvwbYQVOJHv8WA9FCxS7mCAltVl8pKGy1B18"
worksheet_name <- "team"

# Download Google Sheet as a tibble
sheet <- read_sheet(sheet_id, sheet = worksheet_name) 

# Geocode addresses 
sheet_geocoded <- geocode(sheet$Location,output = "latlon")

# Create polygon object for destination
destination <- st_polygon(list(rbind(
  c(18.456331508,-33.954496182),
  c(18.456331508,-33.954496182),
  c(18.456331508,-33.954496182),
  c(18.456331508,-33.954496182))))

get_line<- function(source,destination,segment=100,jitter=0){
  st_sfc(st_linestring(rbind(
    jitter(st_coordinates(source),amount = 0.1), 
    st_coordinates(st_centroid(destination)))),
    crs = 4326) %>% 
    st_segmentize(units::set_units(segment, km))
}

# Join geocoded data back to original sheet
sheet2 <- bind_cols(sheet, sheet_geocoded) %>% 
  filter(!is.na(lat)&!is.na(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  rowwise() %>% 
  mutate(
    id=1:n(),
    line=get_line(geometry,destination,jitter=0.01)) %>% 
  st_set_geometry(NULL) %>% 
  st_set_geometry(value = "line") %>% 
  rename(geometry=line)



sheet2 %>% 
  ggplot(aes(color=id))+ 
  geom_sf()


# Create Leaflet map
leaflet(sheet2) %>%
  addTiles() %>%
  addPolylines(group = id)


sheet2 %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines() 

# Export as shapefile
sheet2 %>% 
  select(project=`PROJECT TITLE`,INSTITUTION) %>% 
  st_write("data/bioscape_team_network.shp",append=F)
