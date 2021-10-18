libs <- c("rgdal", "maptools", "gridExtra")
lapply(libs, require, character.only = TRUE)
library(readr)

#pc <- read_csv("BT postcodes.csv")
pc <- read_csv("postcode_lat_long.csv")

pc <- pc%>%
  filter(lat!=0,
         long!=0,
         !is.na(long),
         !is.na(lat))%>%
  add_count(long,lat)%>%
  filter(n==1)%>%
  
  select(lat,long,name=postcode)%>%
  mutate(
         district=ifelse(
           nchar(name)==7,
           substr(name,start=1,stop=4),
           substr(name,start=1,stop=3)))

 pc1 <- pc%>%
 group_by(district)%>%
 slice_sample(n=100)%>%
   ungroup()
 
count(pc1,district,sort=T)%>%
  tail()

spdf <- SpatialPointsDataFrame(data=pc1,coords = pc1[c(2,1)])
vp <- voronoipolygons(spdf)
vp$pc <- pc1$name
vp$district <- pc1$district
vp <- as(vp,'sf')
st_crs(vp) <- 'WGS84'
vp <- st_intersection(vp,combinedSA)
class(vp)
plot(vp)
leaflet(vp)%>%
  addTiles()%>%
  addPolygons(label = ~pc)

vp1 <- as(vp,'Spatial')

vp2 <- unionSpatialPolygons(vp1,vp1$district)
vp3 <- spTransform(vp2, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leaflet(vp3)%>%
  addTiles()%>%
  addPolygons(label=names(vp3))

vp_write<- as(vp3, "SpatialPolygonsDataFrame")

rgdal::writeOGR(obj = vp_write,dsn = 'vp',driver="ESRI Shapefile", layer='postcode_districts')
#-----------------
pc_sf<- #st_union(
  st_geometry(
    st_centroid(
    st_as_sf(pc1,
             coords = c('long','lat'))))#)

class(pc_sf)
# st_crs(pc_sf) <- 'WGS84'
# plot(pc_sf)
as(pc_sf,'sf')%>%class()

pc_voronoi <- st_voronoi(do.call(c,pc_sf))
class(pc_voronoi)
#pc_voronoi <- st_as_sf(pc_voronoi)
class(pc_voronoi)
plot(pc_voronoi,col=NA)

#pcv <- as(st_collection_extract( pc_voronoi),'Spatial')
#pcv <- st_collection_extract( pc_voronoi)
pcv <- as(pc_voronoi,'sf')
leaflet(pcv)%>%addPolygons()
class(pcv)
pcv <- st_as_sf(pcv)
#st_crs(pc_sf) <- 'WGS84'

# pc_vor <- pc_sf%>%
# as('Spatial')%>%
# voronoipolygons()

# plot(pc_vor)

combinedSA <- st_as_sf(combinedSA)
#as(combinedSA,'Spatial')
#pc_vor <- st_as_sf(pc_vor)
st_crs(combinedSA) <- 'WGS84'
st_crs(pcv) <- 'WGS84'
ni_pc <- st_intersection(pcv,combinedSA)
plot(ni_pc,col=NA)

ni_pc$district <- pc1$district
ni_pc$PC <- rev(pc1$name)
leaflet(ni_pc)%>%
  addTiles()%>%
  addPolygons(label=~PC)

ni_pc <- as(ni_pc,'Spatial')

ni_pc1 <- unionSpatialPolygons(ni_pc,ni_pc$district)
ni_pc1 <- as(ni_pc1,'sf')%>%st_simplify()
ni_pc1$district <- (unique(ni_pc$district))
leaflet(ni_pc1)%>%
  addTiles()%>%
  addPolygons(label= ~district)

# oregon <- readOGR(dsn = "path/to/data", layer = "orcounty")
# oregon.coords <- coordinates(oregon)
# # Generate IDs for grouping
# oregon.id <- cut(oregon.coords[,1], quantile(oregon.coords[,1]), include.lowest=TRUE)
# 
# # Merge polygons by ID
# oregon.union <- unionSpatialPolygons(oregon, oregon.id)
# # Plotting
# plot(oregon)
# plot(oregon.union, add = TRUE, border = "red", lwd = 2)

