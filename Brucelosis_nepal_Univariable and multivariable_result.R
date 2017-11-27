getwd()
setwd("/Users/Tulsigompo/Desktop/brucellosis_data_final")
list.files()
library(dplyr)
library(readr)
bruce_data0<-read_csv("burcelosis_data_modified.csv")
str(bruce_data0)
dim(bruce_data0)

bruce_data0_1<- bruce_data0 %>% 
 mutate(breed = as.factor(breed)) %>%
  mutate(place_of_origin = as.factor(place_of_origin)) %>%
  mutate(brucelosis_status=as.factor(brucelosis_status)) %>%
 mutate (brucelosis_status = factor (brucelosis_status, levels = c ("Positive","Negative"),
                                      labels= c(1,0)))
head(bruce_data0_1)
bruce_data0_1 %>%
  summarize(place_of_origin=n())
table(bruce_data0_1$brucelosis_status)
table(bruce_data0_1$place_of_origin)
table(bruce_data0_1$breed) 


FullModel2 <- glm(brucelosis_status ~ ., family=binomial, data = bruce_data0_1)
summary(FullModel2)
options(na.action = "na.fail") # gives error if missing values
dredge(FullModel1, rank="AIC")
exp(coef(FullModel2))
exp(confint(FullModel2))
library(MuMIn)
library(car)


# Univariable Analysis

Model_3<-glm (brucelosis_status ~  place_of_origin, data = bruce_data0_1,family=binomial(link=logit))
Model_3
summary(Model_3)
exp(coef(Model_3))
exp(confint(Model_3))

Model_4<-glm (brucelosis_status ~ breed, data = bruce_data0_1,family=binomial(link=logit))
Model_4
summary(Model_4)
exp(coef(Model_4))
exp(confint(Model_4))


# Multivariable Analysis after the model selection

Model_2<-glm (brucelosis_status ~ breed + place_of_origin, data = bruce_data0_1,family=binomial(link=logit))
Model_2
exp(coef(Model_2))
exp(confint(Model_2))
# CHisquare test
SumTable<-with(table(breed,brucelosis_status),data=bruce_data0_1)
SumTable
prop.table(SumTable,1)
chisq.test(SumTable)
