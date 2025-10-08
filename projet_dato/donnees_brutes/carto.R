


# library(sf)
library(sp)
library(leaflet)


shpaffich<-"wMain_202205_Nice_secteurs.shp"
filechem<-file.path(shpaffich)
  cd_bar<-st_read(filechem) # equivalent à read_sf package sf
  cd_bar2<-st_zm(cd_bar)
  cd_bar2<-st_transform(cd_bar2,CRS("+init=epsg:4326"))
 
  q2<-leaflet(cd_bar2)  %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines(data=cd_bar2,color="firebrick",weight = 3,
                          opacity = 1.0, fillOpacity = 1,
                          highlightOptions = highlightOptions(color = "white", 
                                                              weight = 2,bringToFront = TRUE),
                          label = cd_bar2$FACILITYID) %>% 
    leaflet::setView(lng=mean(st_bbox(cd_bar2)[c(1,3)]),lat=mean(st_bbox(cd_bar2)[c(2,4)]),zoom=14) # ajout d'un zoom (option)
q2
