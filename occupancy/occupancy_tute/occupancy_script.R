#### Fitting and Interpreting Occupancy Models ####

### Single-season, single species models ###

#Load required packages
library(unmarked)
library(ggplot2)
library(MuMIn)

#Read in the occupancy data (rrp - red-rumped parrots)
rrp<-read.csv("Data/redrump_occu.csv")
rrp

# Read in environmental variables (site covariates)
enviro<-read.csv("Data/enviro_data.csv")
enviro<-enviro[4:12]

## Create unmarked frame object for single species models. 
fumf<- unmarkedFrameOccu(y=rrp[3:5], siteCovs = enviro)
summary(fumf)

## Co-linearity tests
#create a data frame which is just the covariates, not the site data
covar<-enviro 
#generates a matrix of correlation coefficients between each of your covariates
x<-cor(covar, method = "pearson") 
x 
View(x)
## Run a model with constant occupancy 
m1<- occu(~1 ~1, data=fumf)
summary(m1)

## backtransform the estimates for detection (det) and occupancy (state)
backTransform(m1, type = 'det')
backTransform(m1, type = 'state')

## run the full model with variables (on occupancy)
full <- occu(formula = ~ 1 
             ~ Area + Proportioncover + Nearest + Areanearest+CommonCount + RainbowCount + NoisyCount, data = fumf)

summary(full)
#Using the dredge function to rank the models using AIC
modelList <- dredge(full,
                    rank = "AIC")
modelList

## Top performing model 
m2<-occu(~1 ~Area + Nearest + RainbowCount, data = fumf)
summary(m2)

#We can backtransform detection with backTransform but occupancy needs to be transformed with LinearComb or predict

backTransform(m2, type = 'det')

#Set parameters to predict on
orig.area<- seq(1,360,,100)
orig.nearest<-seq(26,1411,,100)
orig.rainbow<-seq(0.5,21.5,,50)

#predict for area, whilst holding the other variables at their mean value

df<-data.frame(Area=103.55, Nearest = orig.nearest, RainbowCount = 12.21)
pred<-round(predict(m2, type = "state", newdata=df, appendData=T),2)
pred

#Plot this
ggplot(pred, aes(x = orig.nearest, y = Predicted) ) +
  xlab("Distance to Nearest Park") + ylim(0,1) +
  ylab("Site occupancy") +
  #geom_point() +
  geom_ribbon( aes(ymin = lower, ymax = upper), alpha = .15) +
  geom_line( aes(y = Predicted), size = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")



#### Multi-species Models

# Read in data for the remaining species
rainbow<-read.csv("Data/lorikeet_occu.csv")
rainbow<-rainbow[3:5]
rainbow

noisy<- read.csv("Data/noisy_miner_occu.csv")
noisy<-noisy[3:5]
noisy

common<- read.csv("Data/common_myna_occu.csv")
common<-common[3:5]
common

##Make a list for the species detection histories

multisp <- list(as.matrix(rrp[3:5]),
                as.matrix(common),
                as.matrix(rainbow),
                as.matrix(noisy))

site.covs<- enviro

#Set up the unmarked frame- this won't work properly and I'll cover why
umf <- unmarkedFrameOccuMulti(y=multisp, siteCovs=site.covs, 
                              obsCovs=NULL) 
umf

umf@fDesign

occformulas<-c('~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1', '~1')
detformulas<-c('~1', '~1', '~1', '~1')

multim1<- occuMulti(detformulas = detformulas,stateformulas=occformulas, data=umf)
multim1

## So what's the issue here?

rainbow
noisy
common


## We can run a multispecies model with common myna without covariates 

multisp2<-list(as.matrix(rrp[3:5]), as.matrix(common))
names(multisp2)<-c('rrp', 'common')

umf2<-unmarkedFrameOccuMulti(y = multisp2, siteCovs=site.covs, obsCovs=NULL)
umf2@fDesign

#Set up formulas for occupancy and detection (without covariates)
occformulas2<-c('~1', '~1', '~1')
detformulas2<-c('~1', '~1')

multim2<-occuMulti(detformulas=detformulas2,stateformulas=occformulas2, data=umf2)
summary(multim2)
#Pred occupancy
lapply(predict(multim2, 'state'), head)
#Pred detection
lapply(predict(multim2, 'det'), head)

#Pred for each species
head(predict(multim2, 'state', species='rrp'))
head(predict(multim2, 'state', species='common'))

#Pred both species occupancy
head(predict(multim2, 'state', species=c('rrp','common')))

## Pred rrp occupancy | presence of common myna
head(predict(multim2,'det',species='rrp',cond='common'))

##Pred rrp occupancy | common absence of common myna
head(predict(multim2,'det',species='rrp',cond='-common')) 
     