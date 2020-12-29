library(dplyr)
library(dlookr)
library(tidyr)
library(lubridate)
station <- read.csv("stations.csv")
aug_2019 <- read.csv("filtered_aug2019.csv")
aug_2020<- read.csv("filtered_aug2020.csv")

#stick coordinates
describe(aug_2019, "Duration")
  
aug_2019_end <- 
  right_join(aug_2019,station, c("EndStation_Id" = "Station.Id"))

aug_2019_end <- aug_2019_end %>%
  select(-c(EndStation_Name,StartStation_Name,StationName,Easting,Northing))

aug_2019_end <- aug_2019_end %>%
  relocate(longitude, latitude, .after = EndStation_Id)

aug_2019_end <- aug_2019_end%>%
  rename(
    d_longitude = longitude,
    d_latitude = latitude
  )
aug_2019_cor <- 
  right_join(aug_2019_end,station, c("StartStation_Id" = "Station.Id")) %>%
  select(-c(StationName,Easting,Northing)) %>%
  rename(
    o_longitude = longitude,
    o_latitude = latitude
  )

aug_2019_cor<-
  aug_2019_cor %>% filter(!is.na(Rental_Id))

write.csv(aug_2019_cor,"aug_2019_cordinates.csv", row.names = FALSE)
#do the same for 2020 data
aug_2020_end <- 
  right_join(aug_2020,station, c("EndStation_Id" = "Station.Id"))

aug_2020_end <- aug_2020_end %>%
  select(-c(EndStation_Name,StartStation_Name,StationName,Easting,Northing))

aug_2020_end <- aug_2020_end %>%
  relocate(longitude, latitude, .after = EndStation_Id)

aug_2020_end <- aug_2020_end%>%
  rename(
    d_longitude = longitude,
    d_latitude = latitude
  )
aug_2020_cor <- 
  right_join(aug_2020_end,station, c("StartStation_Id" = "Station.Id")) %>%
  select(-c(StationName,Easting,Northing)) %>%
  rename(
    o_longitude = longitude,
    o_latitude = latitude
  )

aug_2020_cor<-
  aug_2020_cor %>% filter(!is.na(Rental_Id))

write.csv(aug_2020_cor,"aug_2020_cordinates.csv", row.names = FALSE)
#see top pair
arr = aug_2019_cor %>%
  group_by(StartStation_Id, EndStation_Id) %>%
  mutate(n=n()) %>%
  arrange(desc(n), .by_group=FALSE) %>%
  distinct(StartStation_Id, EndStation_Id,n, .keep_all=TRUE)

arr_top10 = arr %>%
  head(10)

write.csv(arr,"popular_pairs.csv", row.names = FALSE)


#get weekdays
aug_2019_weekday <- aug_2019_cor %>%
  filter(Start_Date!=End_Date)%>%
  mutate(weekday = wday(Start_Date,label = TRUE)) %>%
  filter(!wday(Start_Date) %in% c(1,7))


#get weekends
aug_2019_weekend <- aug_2019_cor %>%
  filter(Start_Date!=End_Date)%>%
  mutate(weekday = wday(Start_Date,label = TRUE)) %>%
  filter(wday(Start_Date) %in% c(1,7))

#checking
aug_2019_weekday%>%
  count(weekday)

aug_2019_weekend%>%
  count(weekday)

#now filter out the loop trips
#weekdays
aug_2019_weekday_loop <- aug_2019_weekday%>%
  filter(StartStation_Id==EndStation_Id)
#weekends
aug_2019_weekend_loop <- aug_2019_weekend%>%
  filter(StartStation_Id==EndStation_Id)

#now have to get the trips per point pair
pair_aug2019_weekday <- aug_2019_weekday %>%
  group_by(StartStation_Id, EndStation_Id) %>%
  mutate(n=n()) %>%
  arrange(desc(n), .by_group=FALSE) %>%
  distinct(StartStation_Id, EndStation_Id,n, .keep_all=TRUE)

pair_aug2019_weekend <- aug_2019_weekend %>%
  group_by(StartStation_Id, EndStation_Id) %>%
  mutate(n=n()) %>%
  arrange(desc(n), .by_group=FALSE) %>%
  distinct(StartStation_Id, EndStation_Id,n, .keep_all=TRUE) %>%
  relocate(StartStation_Id,EndStation_Id, n,.before=X)
#rearrange cols
pair_aug2019_weekday<- pair_aug2019_weekday%>%
  relocate(StartStation_Id,EndStation_Id, n,.before=X)

#attach station name back to network extract
network_extract_copy <- network_extract


library(purrr)
network_extract_copy$vertices<- network_extract_copy$vertices+ 
  purrr::map2(network_extract_copy$vertices$name, station$StationName, ~merge(.x,.y))
#rm unwanted items
rm(aug_2019,aug_2019_end,aug_2020,aug_2020_end)
rm(arr,arr_hotspot,arr_pt,arr_top10)
rm(popup_wkday2019)
#output as csv
write.csv(pair_aug2019_weekday,"pair_2019_weekdays.csv", row.names = FALSE)
write.csv(pair_aug2019_weekend,"pair_2019_weekends.csv", row.names = FALSE)
