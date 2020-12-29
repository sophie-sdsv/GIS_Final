library(spatstat)
library(here)
library(sp)
library(sf)
library(tmaptools)
library(stringr)
library(stplanr)
library(tidyr)
library(pct)
library(dplyr)
library(igraph)
library(leaflet)
library(leafpop)
library(grDevices)
library(colorspace)
#import Borough map
boro <- st_read(here::here("ESRI","London_Borough_Excluding_MHW.shp")) %>%
  st_transform(., 27700)


#import csvs
station <- read.csv("stations.csv")
arr <- read.csv("popular_pairs.csv")

#turning csv into sf object
stations_c <- st_as_sf(station, coords = c("longitude", "latitude"), crs = 4327)
arr_pt <- st_as_sf(arr, coords = c("o_longitude", "o_latitude"), crs=4327)
arr_hotspot <- st_as_sf(arr_top10, coords = c("o_longitude", "o_latitude"), crs=4327) %>%
  st_transform(., 27700)

#getting centroid node
nodes_data = pct::get_centroids_ew() %>% sf::st_transform(27700)

#network testing
#weekdays first

min_flow<-20
net_2019_weekday <- od2line(pair_aug2019_weekday,stations_c)
inter_mvmt_net_2019_wkday <- net_2019_weekday %>% 
  filter(StartStation_Id!=EndStation_Id)%>% 
  filter(n>min_flow)

G=graph.data.frame(inter_mvmt_net_2019_wkday,directed=T)

head(get.edgelist(G))
#plottttt!!!!
plot(G, vertex.label=NA,vertex.size=.5,edge.arrow.size=0.1)
summary(components(G))
head(V(G)$name)
V(G)$x
V(G)$y
E(G)
#get vertices coords
coordVertices=as.data.frame(station[station$Station.Id %in% V(G)$name,],edge.curved=FALSE)
matched=match(V(G)$name, coordVertices[,1])
V(G)$x=coordVertices[matched,3]
V(G)$y=coordVertices[matched,4]

plot(G, vertex.label=NA,vertex.size=1.5, vertex.color = "red", vertex.frame.color='black', edge.width=.1, edge.arrow.size=0.1)

#ok it looks weird, but we need to see in degree and out degree
V(G)$degreeIn <- degree(G,mode = "in")
max(V(G)$degreeIn)
hist(as.numeric(V(G)$degreeIn),breaks=15,
     main="In Degree Frequency")

#plot again
plot(st_geometry(msoa_london), col="transparent")
plot(G, vertex.label=NA,vertex.size=V(G)$degreeIn/10, vertex.color = "blue", vertex.frame.color='black', edge.width=.1, edge.arrow.size=0.1,add=TRUE)

plot(G, vertex.label=NA,vertex.size=V(G)$degreeIn/10, vertex.color = "blue", vertex.frame.color='black', edge.width=.1, edge.arrow.size=0.1)

#NEW stuff here
#try newway of mapping network, im gonna hook it to leaflet
inter_mvmt_net_2019_wkday_reduced <- pair_aug2019_weekday %>%
  select(StartStation_Id,EndStation_Id,n) %>%
  filter(StartStation_Id!=EndStation_Id) %>%
  filter(n>20)
staion_cord <- data.frame("Staion"= station$Station.Id, "lat"=station$latitude,"lon"=station$longitude)

G1 <- graph.data.frame(inter_mvmt_net_2019_wkday_reduced, directed=T, vertices = staion_cord)
plot(G1, vertex.label=NA,vertex.size=V(G1)$degreeIn/10, vertex.color = "blue", vertex.frame.color='black', edge.width=.1, edge.arrow.size=0.1)

V(G1)$degreeIn <- degree(G1, mode = "in")
#add total degree to G1
V(G1)$deg <- degree(G1, mode = "all")
network_extract <-get.data.frame(G1, "both")
vert<- network_extract$vertices
coordinates(vert) <- ~ lon + lat

edges<-network_extract$edges
edges<-lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ],
           vert[vert$name == edges[i, "to"], ]),
     "SpatialLines")
})

for (i in seq_along(edges)) {
  edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
}

edges <- do.call(rbind, edges)

#try adding a cluster to G
#get a new network without directed lines
G_testing=graph.data.frame(inter_mvmt_net_2019_wkday_reduced,directed=T, vertices = staion_cord)
V(G_testing)$degreeIn <- degree(G_testing, mode = "in")
V(G_testing)$degreeOut <- degree(G_testing, mode = "out")
V(G_testing)$deg <- degree(G_testing, mode = "all")
network_extract_GT <-get.data.frame(G_testing, "both")
hist(as.numeric(V(G_testing)$deg),breaks=15,
     main="Degree Frequency")
plot(G_testing, vertex.label=NA,vertex.size=V(G_testing)$deg/10, vertex.color = "blue", vertex.frame.color='black', edge.width=.1, edge.arrow.size=0.1)

#try clustering
cls_GT <- cluster_edge_betweenness(G_testing)
#

#leaflet codes
#paletes
#pal for vert(G1)
pal1 <- colorBin(
  palette = "YlOrRd",
  domain = vert$deg,
  bins = 8 )
#pal for vert(G2)
pal2 <- colorBin(
  palette = "YlOrRd",
  domain = vert_G2$deg,
  bins = 8 )
#pal for vert(G3)
pal3 <- colorBin(
  palette = "YlOrRd",
  domain = vert_G3$deg,
  bins = 8 )
#pal for vert(G4)
pal4 <- colorBin(
  palette = "YlOrRd",
  domain = vert_G4$deg,
  bins = 8 )

  

#leaflet map
#old dont use
leaflet(vert) %>%
  #basemaps
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  addTiles(group = "OSM (default)") %>%
  #Weekday 2019
  #lines of most travelled stations
  addPolylines(data= edges, weight= network_extract$edges$n/100, 
               color = 'purple', group = "weekdays2019") %>%
  #dots of the docks
  addCircles(data=vert, radius=10, weight=network_extract$vertices$deg/10,
             opacity=1,fillOpacity = 0.9,color = ~pal1(vert$deg), group = "weekdays2019") %>%
  #Weekend 2019
  addPolylines(data= edges_G2, weight= network_extract_G2$edges$n/100, 
               color = 'purple', group = "weekends2019") %>%
  addCircles(data=vert_G2, radius=10, weight=network_extract_G2$vertices$degreeIn/5,
             opacity=0.7,fillOpacity = 0.9,color = "midnightblue", group = "weekends2019") %>%
  #Weekday 2020
  addPolylines(data= edges_G3, weight= network_extract_G3$edges$n/100, 
               color = 'purple', group = "weekdays2020") %>%
  addCircles(data=vert_G3, radius=10, weight=network_extract_G3$vertices$degreeIn/5,
             opacity=0.7,fillOpacity = 0.9,color = "skyblue", group = "weekdays2020") %>%
  #Weekend 2020
  addPolylines(data= edges_G4, weight= network_extract_G4$edges$n/100, 
               color = 'purple', group = "weekends2020") %>%
  addCircles(data=vert_G4, radius=10, weight=network_extract_G4$vertices$degreeIn/5,
             opacity=0.7,fillOpacity = 0.9,color = "midnightblue", group = "weekends2020") %>%
  # add a legend

  # specify layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM (default)","Carto (Positron)"),
    overlayGroups = c("weekdays2019","weekends2019","weekdays2020","weekends2020"),
    options = layersControlOptions(collapsed = F) )
  
#THISSSSS
#another one using total degree as vertices
popup_stationName <- stations_c %>%
  st_drop_geometry()%>%
  dplyr::select("Station.Id",'StationName') %>%
  dplyr::rename('Station Name' = StationName) %>%
  popupTable()

leaflet(vert) %>%
  #basemaps
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto (Positron)") %>%
  #Weekday 2019
  #lines of most travelled stations
  addPolylines(data= edges, weight= network_extract$edges$n/50, 
               color =  'purple', group = "weekdays2019") %>%
  #dots of the docks
  addCircles(data=vert, radius=10, weight=network_extract$vertices$deg/10,
             opacity=1,fillOpacity = 1,color = ~pal1(vert$deg), group = "weekdays2019",
             popup = popup_stationName) %>%
  #Weekend 2019
  hideGroup("weekends2019")%>%
  addPolylines(data= edges_G2, weight= network_extract_G2$edges$n/100, 
               color = 'purple', group = "weekends2019") %>%
  addCircles(data=vert_G2, radius=10, weight=network_extract_G2$vertices$deg/3,
             opacity=1,fillOpacity = 1,color = ~pal2(vert_G2$deg), group = "weekends2019",
             popup = popup_stationName) %>%
  #Weekday 2020
  hideGroup("weekdays2020")%>%
  addPolylines(data= edges_G3, weight= network_extract_G3$edges$n/100, 
               color = 'purple', group = "weekdays2020") %>%
  addCircles(data=vert_G3, radius=10, weight=network_extract_G3$vertices$deg/4,
             opacity=1,fillOpacity = 1,color = ~pal3(vert_G3$deg), group = "weekdays2020",
             popup = popup_stationName) %>%
  #Weekend 2020
  hideGroup("weekends2020")%>%
  addPolylines(data= edges_G4, weight= network_extract_G4$edges$n/50, 
               color = 'purple', group = "weekends2020") %>%
  addCircles(data=vert_G4, radius=10, weight=network_extract_G4$vertices$deg/3,
             opacity=1,fillOpacity = 1,color = ~pal4(vert_G4$deg), group = "weekends2020",
             popup = popup_stationName) %>%
  # add a legend
  #wkday 2019
  addLegend(pal = pal1, values = ~'deg', group = "weekdays2019", 
            position ="bottomleft", title = "Total Degree",
            opacity = 0.9) %>%
  #wkend 2019
  addLegend(pal = pal2, values = ~'deg', group = "weekends2019", 
            position ="bottomleft", title = "Total Degree",
            opacity = 0.9) %>%
  #wkday 2020
  addLegend(pal = pal3, values = ~'deg', group = "weekdays2020", 
            position ="bottomleft", title = "Total Degree",
            opacity = 0.9) %>%
  #wkend 2020
  addLegend(pal = pal4, values = ~'deg', group = "weekends2020", 
            position ="bottomleft", title = "Total Degree",
            opacity = 0.9) %>%
  # specify layers control
  addLayersControl(
    baseGroups = c("CartoDB","OSM (default)"),
    overlayGroups = c("weekdays2019","weekends2019","weekdays2020","weekends2020"),
    options = layersControlOptions(collapsed = F) )


#dviz
library(ggplot2)
set_plot_dimensions <- function(width_choice, height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}
set_plot_dimensions(10,2)
boxplot(as.numeric(vert_G4@data[["deg"]]),main = "Total degree for 2020 Weekends", horizontal=T)

