GoatBrucellosis<-read.csv(file.choose(),quote = "'")
GoatBrucellosis$Brucelosis_status<-as.factor(GoatBrucellosis$Brucelosis_status)
GoatBrucellosis$Place_of_Origin<-as.factor(GoatBrucellosis$Place_of_Origin)
GoatBrucellosis$Breed<-as.factor(GoatBrucellosis$Breed)
str(GoatBrucellosis)
GoatBrucellosis


SumTable2<-with(table(Place_of_Origin,Brucelosis_status),data=GoatBrucellosis)
SumTable2
prop.table(SumTable2,1)
SumTable3<-with(table(Breed,Brucelosis_status),data=GoatBrucellosis)
SumTable3

prop.table(SumTable3,1)
# Univariate 
Model2<-glm(Brucelosis_status~Place_of_Origin,data=GoatBrucellosis,family=binomial(link="logit"))
exp(coef(Model2))
(confint(Model2))
exp(confint(Model2))
Model3<-glm(Brucelosis_status~Breed,data=GoatBrucellosis,family=binomial(link="logit"))
exp(coef(Model3))
exp(confint(Model3))
warnings()

# Multivariate
FullModel<-glm(Brucelosis_status~.,data=GoatBrucellosis,family = binomial(link="logit"))
summary(FullModel)
FullModel
#Mumin approach
options(na.action="na.fail")
dredge(FullModel,rank="AIC")
warnings()
Model1<-glm(Brucelosis_status~Breed+Place_of_Origin,data=GoatBrucellosis,family=binomial(link="logit"))
summary(Model1)
exp(coef(Model1))
warnings()
confint(Model1)
exp(confint(Model1))
Anova(Model1,type=3)
cor()
alias()
