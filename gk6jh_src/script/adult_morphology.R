################################################################################
# 
# Paper Title: Shrinking into the big city - Influences of genetic and environmental factors on urban dragon lizard morphology and performance
# Authors: James Baxter-Gilbert, Julia Riley, Celine Frere, Martin Whiting
# Script Description: Testing morphometric differences between urban/natural wild-caught adult dragons
#
# Reviewed 20 April 2020
################################################################################

######################################################################
# Question: Is adult morphology different between urban/natural?
######################################################################

#-----------------------------------------------
# Loading Data (filename = "morphology_all.csv")
data <- read.csv(file="morphology_all.csv", stringsAsFactors = FALSE)
str(data)

# Subset Data to Include Adults Only
data<-data[which(data$dataset=="adult"),]
str(data)

#LOG ALL THE RESPONSE VARIABLES
data$Lsvl<-log10(data$svl)
data$Lforearm<-log10(data$forearm)
data$Lhindlimb<-log10(data$hindlimb)
data$Lfoot<-log10(data$foot)

# Are there missing values?
colSums(is.na(data)) 
# nope, minus what we do not need (age, round, treatment, x_treat, momid & enclosure)

# Sample Sizes
length(data$svl) #197
table(data$sex) #F:123  M:74 
length(unique(data$origin_site)) #7

# Is there sexual dimorphism?
library(lme4)

# 1. Snout-Vent Length
m1<-lmer(Lsvl~sex+urb_cat+(1|origin_site), data=data)
summary(m1)
coefs<-data.frame(coef(summary(m1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

# 2. Forearm Length
m2<-lmer(Lforearm~Lsvl+sex+urb_cat+(1|origin_site), data=data)
summary(m2)
coefs<-data.frame(coef(summary(m2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

# 3. Hindlimb Length
m3<-lmer(Lhindlimb~Lsvl+sex+urb_cat+(1|origin_site), data=data)
summary(m3)
coefs<-data.frame(coef(summary(m3)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

# 4. Hindfoot Length
m4<-lmer(Lfoot~Lsvl+sex+urb_cat+(1|origin_site), data=data)
summary(m4)
coefs<-data.frame(coef(summary(m4)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

# Summary: All morphological traits are larger in males and females.

# Subset Data by Sex
#SUBSET FOR FEMALES ONLY
data_AF<-data[which(data$sex=="F"),]
str(data_AF)

#subset for MALES ONLY
data_AM<-data[which(data$sex=="M"),]
str(data_AM)
#---------------------------------


############### START OF MODELS ###############

# FOR FEMALES ONLY # 

#---------------------------------
# Data exploration for models

#A Are there missing values?
droplevels(data_AF)
colSums(is.na(data_AF))
#no, except for what we do not need (mom_id, age, round, treat, enclo)

#B Are there outliers?
#response variables
dotchart(data_AF$Lsvl,xlab="Range of SVL",ylab="Order of the Data") #one outlier, but will keep in
dotchart(data_AF$Lforearm,xlab="Range of Forearm Length",ylab="Order of the Data")
dotchart(data_AF$Lhindlimb,xlab="Range of Hindlimb Length",ylab="Order of the Data")
dotchart(data_AF$Lfoot,xlab="Range of Hindfoot Length",ylab="Order of the Data")
#predictor variables (categorical)
table(data_AF$urb_cat)
table(data_AF$sex)
#random effects
table(data_AF$origin_site)

#C Is there collinearity?
# NA

#D Relationships Y vs. X
# NA

#E Spatial/temporal aspects of sampling design
# Temporal aspects NA. Spatial aspects are accounted for.

#F Interactions
#NA

#G Zero Inflation Y
sum(data_AF$Lsvl==0)
100*sum(data_AF$Lsvl==0)/nrow(data_AF) 

sum(data_AF$Lforearm==0)
100*sum(data_AF$Lforearm==0)/nrow(data_AF) 

sum(data_AF$Lhindlimb==0)
100*sum(data_AF$Lhindlimb==0)/nrow(data_AF) 

sum(data_AF$Lhindfoot==0)
100*sum(data_AF$Lhindfoot==0)/nrow(data_AF) 


#-----------------------------------------
# Make some models
#library(lme4)

## 1. Snout-Vent Length
#--------------------------
m1<-lmer(Lsvl~urb_cat+(1|origin_site), data=data_AF)
summary(m1)
coefs<-data.frame(coef(summary(m1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m1)
res<-data_AF$Lsvl-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 


## 2. Forearm Length
#--------------------------
m2<-lmer(Lforearm~Lsvl+urb_cat+(1|origin_site), data=data_AF)
summary(m2)
coefs<-data.frame(coef(summary(m2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m2)
res<-data_AF$Lforearm-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 


## 3. Hindlimb Length
#--------------------------
m3<-lmer(Lhindlimb~Lsvl+urb_cat+(1|origin_site), data=data_AF)
summary(m3)
coefs<-data.frame(coef(summary(m3)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m3)
res<-data_AF$Lhindlimb-pred
plot(res~pred)

#checking normality of residuals
hist(res, breaks = 20) 


## 4. Hindfoot Length
#--------------------------
m4<-lmer(Lfoot~Lsvl+urb_cat+(1|origin_site), data=data_AF)
summary(m4)
coefs<-data.frame(coef(summary(m4)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m4)
res<-data_AF$Lfoot-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20) 




#---------------------------------

# FOR MALES ONLY

#---------------------------------
# Data exploration for models

#A Are there missing values?
colSums(is.na(data_AM))
#no, except for what we don't need (mom_id, age, round, treat, enclo)

#B Are there outliers?
#response variables
dotchart(data_AM$Lsvl,xlab="Range of SVL",ylab="Order of the Data")
dotchart(data_AM$Lforearm,xlab="Range of Forearm Length",ylab="Order of the Data")
dotchart(data_AM$Lhindlimb,xlab="Range of Hindlimb Length",ylab="Order of the Data")
dotchart(data_AM$Lfoot,xlab="Range of Hindfoot Length",ylab="Order of the Data")
#predictor variables (categorical)
table(data_AM$urb_cat)
table(data_AM$sex)
#random effects
table(data_AM$origin_site)

#C Is there collinearity?
# NA

#D Relationships Y vs. X
# NA

#E Spatial/temporal aspects of sampling design
# Temporal aspects NA. Spatial aspects are accounted for.

#F Interactions
#NA

#G Zero Inflation Y
sum(data_AM$Lsvl==0)
100*sum(data_AM$Lsvl==0)/nrow(data_AM) 

sum(data_AM$Lforearm==0)
100*sum(data_AM$Lforearm==0)/nrow(data_AM) 

sum(data_AM$Lhindlimb==0)
100*sum(data_AM$Lhindlimb==0)/nrow(data_AM) 

sum(data_AM$Lhindfoot==0)
100*sum(data_AM$Lhindfoot==0)/nrow(data_AM) 


#-----------------------------------------
# Make some models
#library(lme4)

## 1. Snout-Vent Length
#--------------------------
m1<-lmer(Lsvl~urb_cat+(1|origin_site), data=data_AM)
summary(m1)
coefs<-data.frame(coef(summary(m1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m1)
res<-data_AM$Lsvl-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 

#summarize raw data for results description
source(file = "support/summarySE.R")
str(data_AM)
summarySE(data_AM, "svl", "urb_cat") 
DiffNvsU<-232.5882-222.3000
DiffNvsU #10.2882
# % difference in comparison to natural populations
(1-(222.3000/232.5882))*100 #4.423354


## 2. Forearm Length
#--------------------------
m2<-lmer(Lforearm~Lsvl+urb_cat+(1|origin_site), data=data_AM)
summary(m2)
coefs<-data.frame(coef(summary(m2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m2)
res<-data_AM$Lforearm-pred
plot(res~pred)

#checking normality of residuals
hist(res)

#summarize raw data for results description
source(file = "support/summarySE.R")
str(data_AM)
summarySE(data_AM, "forearm", "urb_cat") 
DiffNvsU<-69.46588-68.98050
DiffNvsU #0.48538
# % difference in comparison to natural populations
(1-(68.98050/69.46588))*100 #0.6987315


## 3. Hindlimb Length
#--------------------------
m3<-lmer(Lhindlimb~Lsvl+urb_cat+(1|origin_site), data=data_AM)
summary(m3)
coefs<-data.frame(coef(summary(m3)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m3)
res<-data_AM$Lhindlimb-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20) 

#summarize raw data for results description
source(file = "support/summarySE.R")
str(data_AM)
summarySE(data_AM, "hindlimb", "urb_cat") 
DiffNvsU<-116.4776-113.1402
DiffNvsU #3.3374
# % difference in comparison to natural populations
(1-(113.1402/116.4776))*100 #2.865272


## 4. Hindfoot Length
#--------------------------
m4<-lmer(Lfoot~Lsvl+urb_cat+(1|origin_site), data=data_AM)
summary(m4)
coefs<-data.frame(coef(summary(m4)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m4)
res<-data_AM$Lfoot-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20) 

#summarize raw data for results description
source(file = "support/summarySE.R")
str(data_AM)
summarySE(data_AM, "foot", "urb_cat") 
DiffNvsU<-78.53882-75.15750
DiffNvsU #3.38132
# % difference in comparison to natural populations
(1-(75.15750/78.53882))*100 #4.305285

