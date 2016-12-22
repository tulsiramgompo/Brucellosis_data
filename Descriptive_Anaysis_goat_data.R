library(ggplot2)
library(plyr)
library(dplyr)
library(ggthemes)
library(readr)
getwd()
setwd("~/Desktop/brucellosis_data_final")
getwd()
list.files()




goat1<-read_csv("~/Desktop/brucellosis_data_final/goat_R_2.csv")
goat1<-goat1 %>%
    mutate(goat_breed=factor(goat_breed))
c<-ggplot(goat1,aes(x=goat_breed,y=Positive,alpha=5)) +
  geom_bar(stat = "identity",alpha=0.7) +
  xlab("Goat Breed Category") +
  ylab("RBPT Positive(count)") +
  ggtitle("Rose Bengal Plate Test") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18)) +
  theme_tufte(base_size = 11)
#

library(readxl)
goat2<-read_csv("~/Desktop/brucellosis_data_final/total_goats.csv")
b<-ggplot(goat2,aes(x=test_goats,y=percent,alpha=0.9)) +
  geom_bar(stat = "identity",width = 0.6,alpha=0.7) +
  xlab("RBPT Result") +
  ylab("Goats Tested(%)") +
  ggtitle(" Percent of goats with positive and negative test results") +
theme_tufte(base_size = 13) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100))


library(choroplethr)
library(ggmap)
library(maptools)
library(sp)
install.packages("rgeos")
library(rgeos)
install.packages("gpclib")# may be needed
library(gpclib)
# install.packages("choroplethrMaps")
library(choroplethrMaps)
# Maping goat data through choloropath
goat_map<-read.csv("/Users/Tulsigompo/Desktop/brucellosis_data_final/Map_brucella_nepal_dist.csv")
goat_map<-goat_map %>%
  mutate(NAME_3=as.factor(NAME_3))
nepal_map<-readRDS("NPL_adm3.rds")
#
np_dist<-readRDS("NPL_adm3.rds")
plot(np_dist)

# Choropleth iteration I but not complete
library(ggplot2)
np_dist<-as.data.frame(np_dist) # fortifying to data frame to read as data.(Important)
np_dist <- fortify(np_dist, region = "NAME_3")
ggplot() + geom_map(data = goat_map, aes(map_id = District.of.Nepal, fill = Goat_number), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)


 



