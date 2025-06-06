################################################################################################################################################
# 
# Paper Title: Shrinking into the big city - Influences of genetic and environmental factors on urban dragon lizard morphology and performance
# Authors: James Baxter-Gilbert, Julia Riley, Celine Frere, Martin Whiting
# Script Description: Testing morphometric differences between 2x2 factorial exp at last sampling
#
# Reviewed 20 April 2020
##################################################################################################################################################

####################################################################################
# Question: Is cross-foster morphology different between origin site & treatment?
####################################################################################

#-----------------------------------------------
# Loading Data (filename = "morphology_all.csv")
data <- read.csv(file="morphology_all.csv", stringsAsFactors = FALSE)
str(data)

# Subset Data to Include CrossFoster Only
data<-data[which(data$dataset=="cross_foster"),]
str(data)
table(data$round)
#1  2  3  4  5  6 
#97 91 80 80 77 77 

#LOG ALL THE RESPONSE VARIABLES
data$Lsvl<-log10(data$svl)
data$Lforearm<-log10(data$forearm)
data$Lhindlimb<-log10(data$hindlimb)
data$Lfoot<-log10(data$foot)

# Subset Data to Include Last Round Only
data<-data[which(data$round=="6"),]
dim(data)

# Are there missing values?
colSums(is.na(data)) 

# Sample Sizes
length(data$svl) #77
length(unique(data$mom_id)) #23
length(unique(data$origin_site)) #6
table(data$urb_cat)
length(unique(data$enclo)) #6
table(data$enclo)
table(data$x_treat)

# Age Description
mean(data$age) #371.5325 days
se <- function(x) sd(x)/sqrt(length(x))
se(data$age) # 0.9903127
min(data$age) #351
max(data$age) #386

#---------------------------------


############### START OF MODELS ###############


#---------------------------------
# Data exploration

#A Are there missing values?
droplevels(data)
colSums(is.na(data))


#B Are there outliers?
#response variables
dotchart(data$Lsvl,xlab="Range of SVL",ylab="Order of the Data")
dotchart(data$Lforearm,xlab="Range of Forearm Length",ylab="Order of the Data")
dotchart(data$Lhindlimb,xlab="Range of Hindlimb Length",ylab="Order of the Data")
dotchart(data$Lfoot,xlab="Range of Hindfoot Length",ylab="Order of the Data")
#predictor variables (continuous)
dotchart(data$age,xlab="Range of Age",ylab="Order of the Data")
#predictor variables (categorical)
table(data$x_treat)
#random effects
table(data$origin_site)
table(data$mom_id)
table(data$enclo)

#C Is there collinearity?
# NA

#D Relationships Y vs. X
# NA

#E Spatial/temporal aspects of sampling design
# Temporal aspects NA. Spatial aspects being examined.

#F Interactions
#NA

#G Zero Inflation Y
sum(data$Lsvl==0)
100*sum(data$Lsvl==0)/nrow(data) 

sum(data$Lforearm==0)
100*sum(data$Lforearm==0)/nrow(data) 

sum(data$Lhindlimb==0)
100*sum(data$Lhindlimb==0)/nrow(data) 

sum(data$Lfoot==0)
100*sum(data$Lfoot==0)/nrow(data) 


#-----------------------------------------
# Make some models
library(lme4)

## 1. Snout-Vent Length
#--------------------------
str(data)
m1<-lmer(Lsvl~age+urb_cat*treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
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

# Interaction not significant; re-run model
m1.2<-lmer(Lsvl~age+urb_cat+treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
summary(m1.2)
coefs<-data.frame(coef(summary(m1.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m1.2)
res<-data$Lsvl-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 

#summarize raw data for results description
source(file = "support/summarySE.R")
str(data)
summarySE(data, "svl", "treat") 
DiffNvsU <- 88.10526-83.76923
DiffNvsU #4.33603


## 2. Forearm Length
#--------------------------
m2<-lmer(Lforearm~Lsvl+age+urb_cat*treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
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

# Interaction not significant; re-run model
m2.2<-lmer(Lforearm~Lsvl+age+urb_cat+treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
summary(m2.2)
coefs<-data.frame(coef(summary(m2.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m2.2)
res<-data$Lforearm-pred
plot(res~pred) 

#checking normality of residuals
hist(res)

summarySE(data, "forearm", "treat") 
DiffNvsU<-26.22974-25.14692
DiffNvsU #1.08282
# % difference in comparison to natural
(1-(25.14692/26.22974))*100 #4.128215


## 3. Hindlimb Length
#--------------------------
m3<-lmer(Lhindlimb~Lsvl+age+urb_cat*treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
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

# Interaction not significant; re-run model
m3.2<-lmer(Lhindlimb~Lsvl+age+urb_cat+treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
summary(m3.2)
coefs<-data.frame(coef(summary(m3.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs
format(coefs, scientific=F)

#checking homogeneity of variance
pred<-predict(m3.2)
res<-data$Lhindlimb-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20) 

summarySE(data, "hindlimb", "treat") 
DiffNvsU<-43.03895-41.10692
DiffNvsU #1.93203
# % difference in comparison to natural
(1-(41.10692/43.03895))*100 #4.489027


## 4. Hindfoot Length
#--------------------------
m4<-lmer(Lfoot~Lsvl+age+urb_cat*treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
summary(m4)
coefs<-data.frame(coef(summary(m4)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m4)
res<-data$Lfoot-pred
plot(res~pred)

#checking normality of residuals
hist(res, breaks = 20)

# Interaction not significant; re-run model
m4.2<-lmer(Lfoot~Lsvl+age+urb_cat+treat+(1|origin_site)+(1|mom_id)+(1|enclo), data=data)
summary(m4.2)
coefs<-data.frame(coef(summary(m4.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs
format(coefs, scientific=F)

#checking homogeneity of variance
pred<-predict(m4.2)
res<-data$Lhindlimb-pred
plot(res~pred)

#checking normality of residuals
hist(res, breaks = 20)

summarySE(data, "foot", "treat") 
DiffNvsU<-32.31026-30.89769 
DiffNvsU #1.41257
# % difference in comparison to natural
(1-(30.89769/32.31026))*100 #4.371893

