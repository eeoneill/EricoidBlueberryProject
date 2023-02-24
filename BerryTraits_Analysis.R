#Blueberry Project Data Analysis
#12/22/21
#Berry Data 2019 analysis
#This code will analyze berry mass, berry sugar content, and berry seed count.
#Will use hand pollination treatment, inoculation treatment, and farm as main effects on each berry trait.
#Inoculation treatments included in this analysis will be:
#A (non-inoculated controls)
#C (commercial inoculum)
#E (local soil inoculum).

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

############# Linear Mixed Effects Models #################

library(DHARMa)
library(emmeans)
library(magrittr) 
library(lme4)
library(lmerTest)
library(nlme)

# Berry Mass

MassModel<-lmer(data=BerryData19_mass, 
            mass~Trt
            +BranchTrt
            +Farm
            +(1|PlantNo))
summary(MassModel)
plot(MassModel)
anova(MassModel)
Anova(MassModel)
qqnorm(residuals(MassModel))
emMass <- emmeans(MassModel, specs = pairwise ~ BranchTrt:Trt)
emMass
mean(BerryData19_mass$mass)
install.packages('plotrix')
library(plotrix)
std.error(BerryData19_mass$mass)

# Berry Brix

BrixModel<-lmer(data=BerryData19_brix, 
            brix~Trt
            +BranchTrt
            +Farm
            +(1|PlantNo))
summary(BrixModel)
plot(BrixModel)
qqnorm(residuals(BrixModel))
qqline(residuals(BrixModel))
anova(BrixModel)
emBrix <- emmeans(BrixModel, specs = pairwise ~ Trt:BranchTrt)
emBrix

library(car)
qqPlot(residuals(BrixModel))

# Berry seeds
#Can round to make the values integers...
BerryData19_seeds$Test<-round(BerryData19_seeds$seeds,0)

#if you want to use rounded values, change "seeds" below to "Test"

#GLMER with random values used
SeedModel<-glmer(data=BerryData19_seeds, 
                seeds~Trt
                +BranchTrt
                +Farm
                +(1|PlantNo), family = poisson(link = log))
summary(SeedModel)
library(car)
Anova(SeedModel)
emSeed <- emmeans(SeedModel, specs = pairwise ~ Trt:BranchTrt)
emSeed

#
simulationOutput <- simulateResiduals(fittedModel = SeedModel, 
                                      plot = F)
plot(simulationOutput)

#use data without taking average berry seeds and add PlantNo:BranchTrt 
#as random effect
SeedModel<-glmer(data=BerryData19_RAW, 
            big.med.seeds~Trt
            +BranchTrt
            +(1|Farm)
            +(1|PlantNo)
            +(1|PlantNo:BranchTrt), family=poisson(link = "log"))
summary(SeedModel)
plot(SeedModel)
qqPlot(residuals(SeedModel))
anova(SeedModel)

#
simulationOutput <- simulateResiduals(fittedModel = SeedModel, 
                                      plot = F)
plot(simulationOutput)

#Try summarizing data using the sum over 5 berries instead of average
BerryData19_seeds_SUM<-aggregate(BerryData19_RAW$big.med.seeds, 
                             by=list(BerryData19_RAW$Trt,
                                     BerryData19_RAW$PlantNo,
                                     BerryData19_RAW$BranchTrt,
                                     BerryData19_RAW$BranchCode,
                                     BerryData19_RAW$Farm),
                             FUN = sum)
colnames(BerryData19_seeds_SUM)<-c('Trt',
                                   'PlantNo',
                                   'BranchTrt',
                                   'BranchCode',
                                   'Farm',
                                   'seeds')

SeedModel<-glmer(data=BerryData19_seeds_SUM, 
                 seeds~Trt
                 +BranchTrt
                 +Farm
                 +(1|PlantNo), family=poisson(link = "log"))
summary(SeedModel)
plot(SeedModel)
qqPlot(residuals(SeedModel))
anova(SeedModel)
install.packages('car')
library(car)
Anova(SeedModel)
#
simulationOutput <- simulateResiduals(fittedModel = SeedModel, 
                                      plot = F)
plot(simulationOutput)

#### seed summary stats ####


mean(BerryData19_seeds_SUM$seeds)
mean(BerryData19_seeds$seeds)
library(plotrix)
std.error(BerryData19_seeds$seeds)
Seeds_A<-subset(BerryData19_seeds, BerryData19_seeds$Trt == 'A')
mean(Seeds_A$seeds)
std.error(Seeds_A$seeds)
Seeds_C<-subset(BerryData19_seeds, BerryData19_seeds$Trt == 'C')
mean(Seeds_C$seeds)
std.error(Seeds_C$seeds)
Seeds_E<-subset(BerryData19_seeds, BerryData19_seeds$Trt == 'E')
mean(Seeds_E$seeds)
std.error(Seeds_E$seeds)
###Farms
Seeds_Boutin<-subset(BerryData19_seeds, BerryData19_seeds$Farm == 'Boutin')
mean(Seeds_Boutin$seeds)
Seeds_Adams<-subset(BerryData19_seeds, BerryData19_seeds$Farm == 'Adams')
mean(Seeds_Adams$seeds)
Seeds_Charlotte<-subset(BerryData19_seeds, BerryData19_seeds$Farm == 'Charlotte')
mean(Seeds_Charlotte$seeds)
Seeds_FullBelly<-subset(BerryData19_seeds, BerryData19_seeds$Farm == 'FullBelly')
mean(Seeds_FullBelly$seeds)
std.error(Seeds_FullBelly$seeds)
Seeds_Knoll<-subset(BerryData19_seeds, BerryData19_seeds$Farm == 'Knoll')
mean(Seeds_Knoll$seeds)
std.error(Seeds_Knoll$seeds)
Seeds_Isham<-subset(BerryData19_seeds, BerryData19_seeds$Farm == 'Isham')
mean(Seeds_Isham$seeds)


Seeds_CE<-subset(BerryData19_seeds, Trt == 'E' | BerryData19_seeds$Trt == 'C')
mean(Seeds_CE$seeds)
std.error(Seeds_CE$seeds)

Seeds_HP<-subset(BerryData19_seeds, BranchTrt=='HP')
mean(Seeds_HP$seeds)
std.error(Seeds_HP$seeds)


Seeds_Co<-subset(BerryData19_seeds, BranchTrt=='Co')
mean(Seeds_Co$seeds)
std.error(Seeds_Co$seeds)
##neg binomial ##
library(MASS)
SeedModel<-glmer.nb(data=BerryData19_seeds_SUM, 
                 seeds~Trt
                 +BranchTrt
                 +(1|Farm)
                 +(1|PlantNo))
summary(SeedModel)

simulationOutput <- simulateResiduals(fittedModel = SeedModel, 
                                      plot = F)
plot(simulationOutput)

Anova(SeedModel)

############ Histograms ####################
hist(BerryData19_mass$mass) #normalish skewed
hist(BerryData19_brix$brix) #normalish skewed
hist(BerryData19_seeds$seeds) #poisson

############# Boxplots #########################

#Mass: boxplot for each main effect

boxplot(data=BerryData19_mass, 
        mass~Trt,
        xlab = "Inoculation Treatment",
        ylab = "Mean Berry Mass (g)",
        names=c('No Inoc.','Commercial Inoc.','Local Soil Inoc.'),
        col = c('white'))
boxplot(data=BerryData19_mass, 
        mass~Farm,
        xlab = "Farm",
        ylab = "Mean Berry Mass (g)",
        col = c('white'))
boxplot(data=BerryData19_mass, 
        mass~BranchTrt,
        xlab = "Hand Pollination Treatment",
        ylab = "Mean Berry Mass (g)",
        names = c('Open Pollinated', 'Hand Pollinated'),
        col = c('white'))

#Boxplot for berry mass that includes inoculation treatment and hand poll. treatment

#make labels for x-axis
xLabels=c('No Inoc.\nOpen Poll.',
          'Commercial Inoc.\nOpen Poll.',
          'Local Soil Inoc.\nOpen Poll.',
          'No Inoc.\nHand Poll.',
          'Commercial Inoc.\nHand Poll.',
          'Local Soil Inoc.\nHand Poll.')

boxplot(data=BerryData19_mass, 
        mass~Trt*BranchTrt,
        xlab = "Inoculation and Pollination Treatment",
        ylab = "Mean Berry Mass (g)",
        names = xLabels,
        col = c('white'), 
        las=1,
        par(mgp=c(3,1.5,0)))

#Brix: boxplot for each main effect

boxplot(data=BerryData19_brix, 
        brix~Trt,
        xlab = "Inoculation Treatment",
        ylab = "Mean Berry Sugar Content (Brix)",
        names=c('No Inoc.','Commercial Inoc.','Local Soil Inoc.'),
        col = c('white'))
boxplot(data=BerryData19_brix, 
        brix~Farm,
        xlab = "Farm",
        ylab = "Mean Berry Sugar Content (Brix)",
        col = c('white'))
boxplot(data=BerryData19_brix, 
        brix~BranchTrt,
        xlab = "Hand Pollination Treatment",
        ylab = "Mean Berry Sugar Content (Brix)",
        names = c('Open Pollinated', 'Hand Pollinated'),
        col = c('white'))

#Seeds: boxplot for each main effect

boxplot(data=BerryData19_seeds, seeds~Trt)
boxplot(data=BerryData19_seeds, seeds~Farm)
boxplot(data=BerryData19_seeds, seeds~BranchTrt)

#

