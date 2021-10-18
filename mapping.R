library(leaflet)
library(ggplot2)
library(tidyverse)
#library(ggmap)
library(geojsonio)
library(rgdal)
library(viridis)
library(sf)
library(readxl)

#necessary to mitigate errors resulting from a backend 
#engine change in recent versions
sf::sf_use_s2(FALSE)

#sa <- geojson_read(paste0(getwd(),'/Small Areas.json'),what='sp')
sa <- readRDS('sa.rds') #sp
doh <- read_excel("doh-dd-050521.xlsx", 
    sheet = "Tests by Postal District-7 days")
class(sa)
# leaflet(sa)%>%addTiles()%>%addPolygons()
# leaflet(po3)%>%addTiles()%>%addPolygons()

#construct NI outline
combinedSA <- as(sa,'sf')%>% #method only works for sf objects
  st_buffer(0.01)%>% 
  sf::st_union()

#plot NI outline
leaflet(combinedSA)%>%
  addTiles()%>%
  addPolygons()

class(po3)
class(combinedSA)

combinedSA <- st_as_sf(combinedSA)#as(combinedSA,'sp')#
class(combinedSA)

st_crs(po3) <- 'WGS84'
st_crs(combinedSA)

opc <- st_intersection(combinedSA,po3)
plot(opc)

opc1 <- right_join(doh,opc,by=c('Postal_District'='po1.postcode'))

  labels <- sprintf(
  "<strong>%s</strong><br/>%g people ",
  opc1$Postal_District,opc1$`Positive Cases`) %>%
    lapply(htmltools::HTML)

pal <- leaflet::colorNumeric(palette = 'YlOrRd',domain = opc1$`Positive Cases`)

leaflet(opc)%>%
  addTiles()%>%
  addPolygons(
    fillColor = pal(opc1$`Positive Cases`),
    weight = 2,
  opacity = 1,
  color = "black",
  fillOpacity = 0.7,

   highlight = highlightOptions(sendToBack = T,
    weight = 3,
     color = "white",

    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))%>%
  addLegend(pal = pal,values = opc1$`Positive Cases`,title = 'Positive Cases')%>%
  addCircles(lng = po3$x,lat=po3$y,color = 'black')

plot.new()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(sa,col='grey',border='yellow')
title('Small Areas')
plot(as(combinedSA,'Spatial'),col='blue')
title('Dissolved Small Areas for \n NI border outline')
plot(po3$geometry,col='green')
title('Voronoi Tesselation of \n PC district Centroids')
plot(opc$x,col='pink')
title('Intersection of Voronoi \n constrained by NI outline')
