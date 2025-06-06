################################################################################
# 
# Paper Title: Shrinking into the big city - Influences of genetic and environmental factors on urban dragon lizard morphology and performance
# Authors: James Baxter-Gilbert, Julia Riley, Celine Frere, Martin Whiting
# Script Description: Testing morphometric differences between urban/natural hatchling dragons
#
# Reviewed 20 April 2020
################################################################################

#########################################################################
# Question: Is hatchling morphology different between urban/natural pops?
#########################################################################

#-----------------------------------------------
# Loading Data (filename = "morphology_all.csv")
data <- read.csv(file="morphology_all.csv", stringsAsFactors = FALSE)
str(data)

# Subset Data to Include Hatchlings Only
data<-data[which(data$dataset=="hatchling"),]
str(data)

#LOG ALL THE RESPONSE VARIABLES
data$Lsvl<-log10(data$svl)
data$Lforearm<-log10(data$forearm)
data$Lhindlimb<-log10(data$hindlimb)
data$Lfoot<-log10(data$foot)

# Are there missing values?
colSums(is.na(data)) 
# nope, except what does not apply to this dataset (round, treat, enclo)

# Sample Sizes
length(data$svl) #200
length(unique(data$mom_id)) #40
length(unique(data$origin_site)) #4
table(data$urb_cat)

#---------------------------------


############### START OF MODELS ###############


#---------------------------------
# Data exploration

#A Are there missing values?
droplevels(data)
colSums(is.na(data))
# nope, except what does not apply to this dataset (round, treat, enclo)

#B Are there outliers?
#response variables
dotchart(data$Lsvl,xlab="Range of SVL",ylab="Order of the Data")
dotchart(data$Lforearm,xlab="Range of Forearm Length",ylab="Order of the Data")
dotchart(data$Lhindlimb,xlab="Range of Hindlimb Length",ylab="Order of the Data")
dotchart(data$Lfoot,xlab="Range of Hindfoot Length",ylab="Order of the Data")
#predictor variables (categorical)
table(data$urb_cat)
#random effects
table(data$origin_site)
table(data$mom_id)

#C Is there collinearity?
# NA

#D Relationships Y vs. X
# NA

#E Spatial/temporal aspects of sampling design
# Temporal aspects NA. Spatial aspects are accounted for.

#F Interactions
#NA

#G Zero Inflation Y
sum(data$Lsvl==0)
100*sum(data$Lsvl==0)/nrow(data) 

sum(data$Lforearm==0)
100*sum(data$Lforearm==0)/nrow(data) 

sum(data$Lhindlimb==0)
100*sum(data$Lhindlimb==0)/nrow(data) 

sum(data$Lhindfoot==0)
100*sum(data$Lhindfoot==0)/nrow(data) 


#-----------------------------------------
# Make some models
library(lme4)

## 1. Snout-Vent Length
#--------------------------
m1<-lmer(Lsvl~urb_cat+(1|origin_site)+(1|mom_id), data=data)
summary(m1)
coefs<-data.frame(coef(summary(m1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m1)
res<-data$Lsvl-pred
plot(res~pred)

#checking normality of residuals
hist(res)

#summarize raw data for results description
source(file = "support/summarySE.R")
str(data)
summarySE(data, "svl", "urb_cat") 


## 2. Forearm Length
#--------------------------
m2<-lmer(Lforearm~Lsvl+urb_cat+(1|origin_site)+(1|mom_id), data=data)
summary(m2)
coefs<-data.frame(coef(summary(m2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m2)
res<-data$Lforearm-pred
plot(res~pred)

#checking normality of residuals
hist(res) 


## 3. Hindlimb Length
#--------------------------
m3<-lmer(Lhindlimb~Lsvl+urb_cat+(1|origin_site)+(1|mom_id), data=data)
summary(m3)
coefs<-data.frame(coef(summary(m3)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m3)
res<-data$Lhindlimb-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20) 


## 4. Hindfoot Length
#--------------------------
m4<-lmer(Lfoot~Lsvl+urb_cat+(1|origin_site)+(1|mom_id), data=data)
summary(m4)
coefs<-data.frame(coef(summary(m4)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m4)
res<-data$foot-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20) 
