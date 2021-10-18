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

ni_grid1 <- po%>%
  filter(startsWith(postcode,'BT'))%>%
  select(-id)%>%
rownames_to_column(var = 'id')%>% 
  rename(row=latitude,col=longitude)%>%
mutate(row=scale(row)%>%round(2)*100,
       row=row-min(row)+1,
       col=scale(col)%>%round(2)*100,
       col=col-min(col)+1)%>%
  distinct(row,col,.keep_all=T)#%>%
  #filter(startsWith(postcode,'BT1'))

nrow(ni_grid1)  

#grid_design(ni_grid1)
grid_preview(geofacet::ie_counties_grid1)
mygrid <- data.frame(
  code = c("28", "29", "27", "78", "25", "54", "24", "26", "67", "62", "55", "68", "80", "57", "18", "56", "17", "69", "63", "70", "59", "19", "60", "79", "77", "73", "58", "15", "61", "21", "20", "66", "65", "64", "53", "16", "2", "9", "71", "1", "45", "12", "3", "8", "22", "72", "34", "4", "14", "74", "5", "7", "10", "11", "6", "23", "75", "13", "30", "32", "36", "31", "33", "40", "76", "35", "37", "41", "38", "39", "42", "43", "44", "46", "48", "47", "50", "51", "49", "52"),
  name = c("BT34", "BT35", "BT33", "BT92", "BT31", "BT60", "BT30", "BT32", "BT74", "BT68", "BT61", "BT75", "BT94", "BT63", "BT25", "BT62", "BT24", "BT76", "BT69", "BT77", "BT65", "BT26", "BT66", "BT93", "BT9", "BT8", "BT64", "BT22", "BT67", "BT28", "BT27", "BT71", "BT70", "BT7", "BT6", "BT23", "BT10", "BT17", "BT78", "BT1", "BT5", "BT2", "BT11", "BT16", "BT29", "BT79", "BT4", "BT12", "BT21", "BT80", "BT13", "BT15", "BT18", "BT19", "BT14", "BT3", "BT81", "BT20", "BT36", "BT38", "BT41", "BT37", "BT39", "BT45", "BT82", "BT40", "BT42", "BT46", "BT43", "BT44", "BT47", "BT48", "BT49", "BT51", "BT53", "BT52", "BT55", "BT56", "BT54", "BT57"),
  row = c(1, 5, 27, 28, 50, 56, 72, 73, 76, 85, 86, 89, 89, 94, 97, 101, 106, 107, 109, 110, 118, 120, 122, 125, 127, 128, 131, 140, 141, 142, 143, 144, 145, 146, 150, 158, 162, 162, 168, 171, 174, 175, 180, 187, 188, 189, 190, 191, 194, 197, 204, 209, 210, 211, 213, 215, 217, 227, 231, 231, 232, 237, 237, 243, 269, 278, 281, 285, 303, 329, 329, 343, 358, 362, 373, 393, 402, 418, 419, 420),
  col = c(288, 242, 338, 64, 325, 206, 372, 277, 29, 172, 211, 90, 51, 255, 301, 234, 342, 112, 154, 125, 260, 309, 280, 1, 333, 359, 244, 404, 280, 299, 316, 197, 165, 336, 363, 379, 315, 278, 80, 334, 347, 300, 283, 370, 257, 110, 356, 327, 400, 189, 293, 346, 362, 387, 318, 333, 38, 402, 294, 355, 274, 337, 317, 208, 70, 351, 271, 204, 271, 270, 108, 88, 154, 201, 237, 206, 184, 206, 275, 229),
  stringsAsFactors = FALSE
)
#takes ages
#geofacet::grid_preview(mygrid)

po1 <- po%>%
  filter(startsWith(postcode,'BT'))%>%
  select(-id)%>%
  rownames_to_column(var = 'id')%>%
  rename(name=id,col=latitude,row=longitude)

#counties <- as(counties, "Spatial")
#as(po1,'Spatial')

po2 <- st_as_sf(po1,coords = c('row','col'))%>%
  as('Spatial')%>%
  #st_voronoi(st_multipoint( as.matrix(po1[3:4])))%>%
  #st_collection_extract()
  voronoipolygons()

po3 <- po2%>%as(.,'sf')%>%
  mutate(po1$name,po1$postcode)

filtered <- sample(c(F,T),size = 80,replace=TRUE,prob = c(0.1,0.9))
geo_pc <- geofacet::grid_auto(po3[filtered,])

# geo_pc%>%mutate(code=1:nrow(.),
# name=po1$postcode[filtered])%>%
geofacet::grid_preview(geo_pc,label = 'name_po1.postcode')

(shortpc_geo <- ggplot(doh,aes(fill=`Positive Cases`,x=1, y=`Positive Cases`),
       show.legend=F)+
  geom_col(show.legend = F,position='stack')+
  scale_fill_gradient(low = 'green',high='red')+
  theme_void()+coord_polar()+
  geom_text(mapping = aes(1,`Positive Cases`,label=`Positive Cases`),nudge_y = 0.5)+
  #facet_wrap(~Postal_District)
  geofacet::facet_geo(facets = ~Postal_District,grid = geo_pc))


