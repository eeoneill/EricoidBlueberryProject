#Make ggplot figures for berry traits in 2019:

#read in data
BerryData19_RAW<-read.csv("berryData2019-toAnalyze.csv")

# avoid pseudoreplication
# take the mean berry mass for fruits that were hand pollinated and plants that were

#Mass 
BerryData19_mass<-aggregate(BerryData19_RAW$mass, 
                            by=list(BerryData19_RAW$Trt,
                                    BerryData19_RAW$PlantNo,
                                    BerryData19_RAW$BranchTrt,
                                    BerryData19_RAW$Farm),
                            FUN = mean)
colnames(BerryData19_mass)<-c('Trt','PlantNo','BranchTrt','Farm','mass')

#Brix
BerryData19_brix<-aggregate(BerryData19_RAW$brix, 
                            by=list(BerryData19_RAW$Trt,
                                    BerryData19_RAW$PlantNo,
                                    BerryData19_RAW$BranchTrt,
                                    BerryData19_RAW$Farm),
                            FUN = mean)
colnames(BerryData19_brix)<-c('Trt','PlantNo','BranchTrt','Farm','brix')

#Seeds
BerryData19_seeds<-aggregate(BerryData19_RAW$big.med.seeds, 
                             by=list(BerryData19_RAW$Trt,
                                     BerryData19_RAW$PlantNo,
                                     BerryData19_RAW$BranchTrt,
                                     BerryData19_RAW$Farm),
                             FUN = mean)
colnames(BerryData19_seeds)<-c('Trt','PlantNo','BranchTrt','Farm','seeds')


##### FIGURES #######
library(ggplot2)

#Berry mass and berry seed have significant data:
#Get interaction so you have correct data to plot
BerryData19_mass$MycBranchTrt<-interaction(BerryData19_mass$Trt,
                                           BerryData19_mass$BranchTrt)
BerryData19_seeds$MycBranchTrt<-interaction(BerryData19_seeds$Trt,
                                           BerryData19_seeds$BranchTrt)
#Berry mass graph in color:
m<-ggplot(data=BerryData19_mass,aes(x=MycBranchTrt, y=mass, fill=Trt)) +
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(outlier.colour = "black",outlier.size = 1)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 12))+
  scale_fill_manual(values=c('#FFB90F','#CD2626','#1874CD'))
m
#berry seed graph B&W
m2<-ggplot(data=BerryData19_mass,aes(x=MycBranchTrt, y=mass)) +
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(outlier.colour = "black",outlier.size = 1)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 12))
m2

#berry seed graph in color:
s<-ggplot(data=BerryData19_seeds,aes(x=MycBranchTrt, y=seeds, fill=Trt)) +
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(outlier.colour = "black",outlier.size = 1)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 12))+
  scale_fill_manual(values=c('#FFB90F','#CD2626','#1874CD'))
s

#berry seed graph in B&W:
s2<-ggplot(data=BerryData19_seeds,aes(x=MycBranchTrt, y=seeds)) +
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(outlier.colour = "black",outlier.size = 1)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 12))
s2


