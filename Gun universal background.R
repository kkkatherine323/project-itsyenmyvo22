
library(dplyr)
library(tidyverse)
library(ggplot2)
data <- read.csv("~/info201/projects/project-itsyenmyvo22/data/universal.csv", stringsAsFactors = F)
View(data)
guns <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-itsyenmyvo22/main/data/all_sum_df.csv", stringsAsFactors = F)%>%
  filter(date > '01-01-2017')%>%
  select(state, n_killed)
  
final_data <- left_join(data, guns, by= "state")

View(final_data)

state_universal <- final_data %>%
  group_by(state)%>%
 filter(year== 2017)%>%
  filter(universal == 1)%>%
    distinct(state)%>%
    pull(state)
 print(state_universal)

 
# all data loaded
 
#finding background check table
 state_univ_thing <- final_data %>%
   mutate(state = tolower(state))%>%
   select(universal, state)

 

 View(state_univ_thing)
 

 
 #finding deaths
 
 people_killed <- final_data %>%
   group_by(state)%>%
   summarize (deaths = sum(n_killed))%>%
   mutate(state= tolower(state))
 View(people_killed)

 ##coordinate data
 coordinates_50states <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-itsyenmyvo22/main/data/coordinates_50states2.csv", stringsAsFactors = FALSE) %>%
   select(location, lat, lon)
 View(coordinates_50states)
 names(coordinates_50states)[names(coordinates_50states) == "location"] <- "state"
 names(coordinates_50states)[names(coordinates_50states) == "lat"] <- "lat"
 names(coordinates_50states)[names(coordinates_50states) == "lon"] <- "long"
 coordinates_50states <- coordinates_50states %>%
   mutate(state = tolower(state)) 
 View(coordinates_50states)
 
 coords_killed <- left_join(people_killed, coordinates_50states, by = "state")%>% 
 add_row(state = "south dakota", deaths = 90, lat = 43.969515, long = -99.901813) %>%
   add_row(state = "west virginia", deaths = 335, lat = 38.597626, long = -80.454903)
 
 View(coords_killed)
 
 killed <- coords_killed %>%
   summarise(deaths = deaths/60)
 View(killed)
 
 ##creating the map where it shows gun purchases
  
 state_shape <- map_data("state")%>%
   rename(state=region)%>%
   left_join(state_univ_thing, by="state")

   

ggplot(state_shape)+
  geom_polygon(
    mapping = aes(x= long, y=lat, group=group, fill= universal),
    color= "white", 
    size=0.1
  )+
  coord_map()+
  scale_fill_continuous(low= "Red", high= "Blue")+
  labs(fill= "age18longgunsale")+ 
  geom_point(
    data = coords_killed,
    mapping = aes(x = long, y = lat),
    color = "yellow",
    size = killed$deaths
  ) +
  labs(title = "Proportion of Gun Deaths in States")

