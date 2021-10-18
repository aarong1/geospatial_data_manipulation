library(tidyverse)
library(geofacet)
library(sf)
library(readxl)

po <- read_csv("postcode-outcodes.csv")
# doh <- read_excel("~/Downloads/doh-dd-300421.xlsx", 
#     sheet = "Tests by Postal District-7 days")

voronoipolygons = function(layer) {
    require(deldir)
    crds = layer@coords
    z = deldir(crds[,1], crds[,2])
    w = tile.list(z)
    polys = vector(mode='list', length=length(w))
    require(sp)
    for (i in seq(along=polys)) {
        pcrds = cbind(w[[i]]$x, w[[i]]$y)
        pcrds = rbind(pcrds, pcrds[1,])
        polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
    SP = SpatialPolygons(polys)
    voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
        y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
        function(x) slot(x, 'ID'))))
}

po1 <- po%>%
  filter(startsWith(postcode,'BT'))%>%
  select(-id)%>%
  rownames_to_column(var = 'id')%>%
  rename(name=id,col=latitude,row=longitude)

po2 <- st_as_sf(po1,coords = c('row','col'))%>%
  as('Spatial')%>%
  #st_voronoi(st_multipoint( as.matrix(po1[3:4])))%>%
  #st_collection_extract()
  voronoipolygons()

po3 <- po2%>%as(.,'sf')%>%
  mutate(po1$name,po1$postcode)

