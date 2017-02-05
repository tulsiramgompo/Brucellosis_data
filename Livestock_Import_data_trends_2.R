library(readr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(ggthemes)
getwd()
setwd("/Users/Tulsigompo/Desktop/Brucellosis_Project/R_projects/Brucellosi_data")
trend_data<-read_csv("Livestcock_import_Nepal_2.csv")
trend_data<-trend_data %>% 
group_by(year_cat,counts,animals) %>%
summarize (per_ten_thou= mean(counts/10000)) 
# Trend analysis
plot1<-ggplot(trend_data, aes(x = year_cat, y = per_ten_thou, group =animals,shape=animals,color=animals)) +
  geom_point (color="red",size=3) + 
  xlab("Year") + ylab ("Number of animals per ten thousands ") +
  geom_line(size= 1.5) + scale_y_continuous(breaks= c(5,10,15,20,25,30,35,40,45,50)) +
  ggtitle("Trends of Importation of Livestocks to Nepal from India and China") +
  theme_few()
plot1
# Descriptive statistics
summary(trend_data$counts)
summary(trend_data)
#box_plot
trend_data_2<-trend_data %>% 
  ggplot(aes(x=animals,y=per_ten_thou)) +
  geom_boxplot(outlier.color = "red",outlier.shape = 1) +  
  theme_few() 
trend_data_2
#alternative way with year_cat(not well suited)
trend_data_3<-trend_data %>% 
  ggplot(aes(x=year_cat,y=per_ten_thou,group=animals)) +
  geom_boxplot(outlier.color = "red",outlier.shape = 1) +  
  theme_few() 
trend_data_3
# with geom_point(not suitable)
trend_data_4<-trend_data %>% 
  ggplot(aes(x=animals,y=per_ten_thou)) +
  geom_point() +  
  theme_few() 
trend_data_4

# descriptive stistics with changing data structure
trend_data_1<-read_csv("Livestcock_import_Nepal_1.csv")
mean_buff<-mean(trend_data_1$buffaloes)
mean_goat<-mean(trend_data_1$goat)
summary(trend_data_1)
# with geom_point
trend_data_1<-trend_data_1 %>% 
  ggplot(aes(x=goat,y=buffaloes )) +
  geom_point(size=2,color="red",alpha=0.6) +  
geom_smooth(method = "lm")
  theme_few() 
trend_data_1



