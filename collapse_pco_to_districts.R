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