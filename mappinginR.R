library(ggmap)
library(tidyverse)
library(maps)
library(rbokeh)
library(sf)
library(mapview)
?ggmap
(map <- get_map(c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)))
ggmap(map)

(map <- get_map(maptype = "roadmap"))
(map <- get_map(source = "osm"))
(map <- get_map(source = "stamen", maptype = "watercolor"))

map <- get_map(location = "texas", zoom = 6, source = "stamen")

ggmap(map, fullpage = TRUE)


ggmap(ggmap = get_map(source='osm'))
names(world.cities)
ggplot(df)+geom_sf()

library(mapview)
library(sf)
mapview(franconia, color = "white", col.regions = "red")
  mapview(franconia, color = "magenta", col.regions = "white")

  mapview(breweries, zcol = "founded")
  mapview(breweries, zcol = "founded", at = seq(1400, 2200, 200), legend = TRUE)
  mapview(franconia, zcol = "district", legend = TRUE)

  clrs <- sf.colors
  mapview(franconia, zcol = "district", col.regions = clrs, legend = TRUE)

mapview(df) + breweries
  mapview(list(breweries, franconwcia))
  mapview(franconia) + mapview(breweries) + trails

  mapview(franconia, zcol = "district") + mapview(breweries, zcol = "village")
  mapview(list(franconia, breweries),
          zcol = list("district", NULL),
          legend = list(TRUE, FALSE))
#library(mapdeck)
#mapviewOptions(platform = 'mapdeck')
mapviewOptions(platform = "leaflet")
mapview::mapView(world.cities[sample(nrow(world.cities),500),],xcol='long',ycol='lat')

library(gt)
gt(world.cities%>%head(20),groupname_col = 'country.etc',auto_align = F,)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
df <- st_as_sf(x = world.cities[sample(1:nrow(world.cities),100),],                         
           coords = c("long", "lat"),
           crs = projcrs)
mapview(df)


mu <- c(-95.3632715, 29.7632836); nDataSets <- sample(4:10,1)
chkpts <- NULL
for(k in 1:nDataSets){
  a <- rnorm(2); b <- rnorm(2);
  si <- 1/3000 * (outer(a,a) + outer(b,b))
  chkpts <- rbind(
    chkpts,
    cbind(MASS::mvrnorm(rpois(1,50), jitter(mu, .01), si), k)
  )
}
chkpts <- data.frame(chkpts)
names(chkpts) <- c("lon", "lat","class")
chkpts$class <- factor(chkpts$class)
qplot(lon, lat, data = chkpts, colour = class)

(hdf <- get_map(c(left = -95.7568, bottom = 29.3, right = -95.099334, top = 30.29652)))

ggmap(hdf, extent = "normal") +
  geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)

#--------------

# list of all file names with time stamp 2020-01-07 21:00 GMT 
# (BOM images are retained for 24 hours, so this will return an
# empty vector if you run this code without editing the time stamp)
(files <- bomrang::get_available_imagery() %>%
  stringr::str_subset("202012280000"))

# use curl_download() to obtain a single file, and purrr to 
# vectorise this operation
purrr::walk2(
  .x = paste0("ftp://ftp.bom.gov.au/anon/gen/gms/", files),
  .y = file.path("raster", files),
  .f = ~ download.file(url = .x, destfile = .y)
)
dir('raster')
img_vis  <- file.path("raster", "IDE00422.202012280000.tif")
img_inf <- file.path("raster", "IDE00421.202012280000.tif")

library(stars)

#> Loading required package: abind
sat_vis <- read_stars(img_vis, RasterIO = list(nBufXSize = 600, nBufYSize = 600))
sat_inf <- read_stars(img_inf, RasterIO = list(nBufXSize = 600, nBufYSize = 600))

sat_vis
library(tidyverse)
ggplot() + 
  geom_stars(data = sat_inf) + 
  coord_equal()+theme_void()
