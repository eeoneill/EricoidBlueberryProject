#Blueberry Project data analysis
#number of plants that flowered 
#ANOVA
#myc and year and main affects

#ANOVA with all treatments, A, B, C, D, E
FlowerPlants<-read.csv("NoPlantsFlowered.csv")

FlowerPlantsANOVA<-aov(FlowerPlants$Flowered~FlowerPlants$MycTrt+FlowerPlants$Year)
summary(FlowerPlantsANOVA)

#ANOVA without peat control (B)

FlowerPlants<-read.csv("NoPlantsFloweredACDE.csv")

FlowerPlantsANOVA<-aov(FlowerPlants$Flowered~FlowerPlants$MycTrt+FlowerPlants$Year)
summary(FlowerPlantsANOVA)

#letters
library(multcompView)
FlowerPlants$MycTrtYear <- interaction(FlowerPlants$MycTrt, FlowerPlants$Year)

FlowerPlantsANOVA<-aov(FlowerPlants$Flowered~FlowerPlants$MycTrtYear)

Output = TukeyHSD(FlowerPlantsANOVA)
PT4 = Output$`FlowerPlants$MycTrtYear`[,'p adj']
multcompLetters(PT4)

### Figure ###
library(ggplot2)
library(dplyr)
Counts<-FlowerPlants %>% dplyr::count(MycTrt, Flowered, Year)
Counts.filtered<-filter(Counts,Flowered==0)
Counts.filtered$Proportion<-Counts.filtered$n/90
Counts.filtered$MycTrtYear<-interaction(Counts.filtered$MycTrt,Counts.filtered$Year)

barplot(Counts.filtered$Proportion~Counts.filtered$MycTrtYear)

library(plotrix)


p<- ggplot(Counts.filtered, aes(x=MycTrtYear, y=Proportion, fill=MycTrt)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge())

p+labs(x="Myc Trt", y = "Proportion PlantsFlowered")+
  theme_classic() +
  scale_fill_manual(values=c('#FFB90F','#BCEE68','#1874CD','#CD2626'))

#B&W
#install.packages('ggpattern')
library(ggpattern)

p<- ggplot(Counts.filtered, aes(x=MycTrtYear, y=Proportion, fill=MycTrt)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge())
p+theme_classic()+ scale_fill_manual(values=c('white', 'darkgrey', 'white','white'))

p<- ggplot(Counts.filtered,aes(x=MycTrtYear, y=Proportion, fill=MycTrt)) +
  geom_col_pattern(aes(pattern = MycTrt), width = 0.8,
                   color = "black", pattern_fill = "white", 
                   pattern_angle = 45, 
                   pattern_density = 0.1, 
                   pattern_spacing = 0.025, 
                   pattern_key_scale_factor = 0.6)+
  scale_fill_manual(values=c('white', 'white', 'darkgrey','white'))+
  scale_pattern_manual(values = c("none", "stripe",'none','circle')) +
  theme_classic()
    
p

p<- ggplot(Counts.filtered, aes(x=MycTrtYear, y=Proportion, fill=MycTrt)) + 
  geom_bar_pattern(stat = "identity", color = 'black',position=position_dodge(),
                   pattern_fill = "black",
                   aes(pattern = MycTrt))+
  scale_fill_manual(values=c('white','black','white','white'))
  

p+theme_classic()
