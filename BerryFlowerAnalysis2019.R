#Blueberry Project 2019 analysis
#Created Oct 25, 2022
#Erin O'Neill
#This code will analyze the total number of berries produced and flowers

BerryRAW<-read.csv('BerriesCollected2019.csv')
head(BerryRAW)

BerryData<-subset(BerryRAW, BerryRAW$Treatment=='A')
tail(BerryData)

BerryData<-aggregate(BerryRAW$NumBerries,
                     by = list(BerryRAW$Treatment, BerryRAW$PlantNo, BerryRAW$Farm), 
                     FUN = sum)
head(BerryData)
colnames(BerryData)<-c('Trt','PlantNo','Farm','NumBerries')

library(lmerTest)

NumBerriesGLM<-glmer(data=BerryData,
                   NumBerries~Trt
                   +Farm
                   +(1|PlantNo), family = poisson(link = 'log'))
summary(NumBerriesGLM)
library(car)
Anova(NumBerriesGLM)
