library(dplyr)
library(dlookr)
library(tidyr)
library(lubridate)
#create igraph objects for the other 3 dataset
inter_mvmt_2020_wkend <- pair_aug2020_weekend %>%
  select(StartStation_Id,EndStation_Id,n) %>%
  filter(StartStation_Id!=EndStation_Id) %>%
  filter(n>20)
#no need to change this
staion_cord <- data.frame("Staion"= station$Station.Id, "lat"=station$latitude,"lon"=station$longitude)

#continue make network, just find and replace 
G4 <- graph.data.frame(inter_mvmt_2020_wkend, directed=T, 
                       vertices = staion_cord)
V(G4)$degreeIn <- degree(G4, mode = "in")
V(G4)$degreeOut <- degree(G4, mode = "out")
V(G4)$deg <- degree(G4, mode = "all")
network_extract_G4 <-get.data.frame(G4, "both")
vert_G4<- network_extract_G4$vertices
coordinates(vert_G4) <- ~ lon + lat
edges_G4<-network_extract_G4$edges
edges_G4<-lapply(1:nrow(edges_G4), function(i) {
  as(rbind(vert_G4[vert_G4$name == edges_G4[i, "from"], ],
           vert_G4[vert_G4$name == edges_G4[i, "to"], ]),
     "SpatialLines")
})

for (i in seq_along(edges_G4)) {
  edges_G4[[i]] <- spChFIDs(edges_G4[[i]], as.character(i))
}

edges_G4 <- do.call(rbind, edges_G4)

#creae 2020 datas
aug_2020_weekday <- aug_2020_cor %>%
  filter(Start_Date!=End_Date)%>%
  mutate(weekday = wday(Start_Date,label = TRUE)) %>%
  filter(!wday(Start_Date) %in% c(1,7))


#get weekends
aug_2020_weekend <- aug_2020_cor %>%
  filter(Start_Date!=End_Date)%>%
  mutate(weekday = wday(Start_Date,label = TRUE)) %>%
  filter(wday(Start_Date) %in% c(1,7))

#get non loop trips
pair_aug2020_weekday <- aug_2020_weekday %>%
  group_by(StartStation_Id, EndStation_Id) %>%
  mutate(n=n()) %>%
  arrange(desc(n), .by_group=FALSE) %>%
  distinct(StartStation_Id, EndStation_Id,n, .keep_all=TRUE) %>%
  relocate(StartStation_Id,EndStation_Id, n,.before=X)

pair_aug2020_weekend <- aug_2020_weekend %>%
  group_by(StartStation_Id, EndStation_Id) %>%
  mutate(n=n()) %>%
  arrange(desc(n), .by_group=FALSE) %>%
  distinct(StartStation_Id, EndStation_Id,n, .keep_all=TRUE) %>%
  relocate(StartStation_Id,EndStation_Id, n,.before=X)


#singleuse
V(G3)$deg <- degree(G3, mode = "all")
V(G3)$deg <- degree(G3, mode = "all")
V(G4)$deg <- degree(G4, mode = "all")
network_extract_G4 <-get.data.frame(G4, "both")
vert_G4<- network_extract_G4$vertices
coordinates(vert_G4) <- ~ lon + lat
