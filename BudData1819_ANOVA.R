#read in data
BudDataF19<- read.csv(file ="BudCountsF19.csv",fileEncoding = 'UTF-8-BOM')
BudDataF18<- read.csv(file ="BudCountsS2019.csv",fileEncoding = 'UTF-8-BOM')

#analyze bud data

#create a table with all data
#add year to data
BudDataF18$year<-'2018'
BudDataF19$year<-'2019'

total<-rbind(BudDataF18,BudDataF19)

###### Histogram #########

hist(total$TotalBuds)

##### ANOVA ###########

BudAnova<-aov(data=total,
              TotalBuds~Trt
              +year
              #+Trt*year
              )
summary(BudAnova)
TukeyHSD(BudAnova)


############## Generalized Linear Model ############

BudGLM<-glm(data=total,
              TotalBuds~Trt
              +year
              +Trt*year,
              family = poisson()
              )
summary(BudGLM)
library(car)

Anova(BudGLM)

#letters
library(multcompView)

BUD18<-aov(BudDataF18$TotalBuds~BudDataF18$Trt)

Output = TukeyHSD(BUD18)
PT4 = Output$`BudDataF18$Trt`[,'p adj']
multcompLetters(PT4)

BUD19<-aov(BudDataF19$TotalBuds~BudDataF19$Trt)

Output = TukeyHSD(BUD19)
PT4 = Output$`BudDataF19$Trt`[,'p adj']
PT4
multcompLetters(PT4)



Output = TukeyHSD(BudAnova)
PT4 = Output$Trt[,'p adj']
PT4
multcompLetters(PT4)

########### BOXPLOT ###############
xLabels<-c('No\nInoc.',
           'Commercial\nInoc.',
           'Combination\nSoil Inoc.',
           'Local\nSoil',
           'No\nInoc.',
           'Commercial\nInoc.',
           'Combination\nSoil Inoc.',
           'Local\nSoil Inoc.')

Bud1819_boxplot<-boxplot(data=total,
                         TotalBuds~Trt*year,
                         xlab = "Inoculation Treatment and Year",
                         ylab = "Total Bud Count",
                         names = xLabels,
                         col = c('white'), 
                         las=1,
                         par(mgp=c(3.25,1.5,0)))

#boxplot in ggplot
library(ggplot2)
total$TrtYear<-interaction(total$Trt,total$year)

s2<-ggplot(data=total,aes(x=TrtYear, y=TotalBuds)) +
  stat_boxplot(geom ='errorbar', width = 0.2)+
  geom_boxplot(outlier.colour = "black",outlier.size = 1)+
  theme_classic()+
  theme(axis.text.y = element_text(size = 12))
s2
