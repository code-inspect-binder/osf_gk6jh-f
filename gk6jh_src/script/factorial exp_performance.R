################################################################################################################################################
# 
# Paper Title: Shrinking into the big city - Influences of genetic and environmental factors on urban dragon lizard morphology and performance
# Authors: James Baxter-Gilbert, Julia Riley, Celine Frere, Martin Whiting
# Script Description: Testing performance differences between 2x2 factorial dragons
#
# Reviewed 20 April 2020
################################################################################


#-------------------------------------------------------------------------------#
# Does gripping ability differ between substrates?        # 
#-------------------------------------------------------------------------------#

#-----------------------------------------------------
#Loading Data (filename = "gripping_performance.csv")
data2<-read.csv(file="gripping_performance.csv")
str(data2)
data2$id<-as.factor(data2$id)
data2$Lsvl<-log10(data2$svl)


# Make Model
#-----------
library(lme4)

mS1<-lmer(measure ~ Lsvl+order+b.temp+type+urb.cat*treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data = data2)
summary(mS1)
coefs<-data.frame(coef(summary(mS1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(mS1)
res<-data2$measure-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20)

# Interaction not significant; re-run model for reporting
mS1.2<-lmer(measure ~ Lsvl+order+b.temp+type+urb.cat+treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data = data2)
summary(mS1.2)
coefs<-data.frame(coef(summary(mS1.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(mS1.2)
res<-data2$measure-pred
plot(res~pred) 

#checking normality of residuals
hist(res, breaks = 20)

# yes, gripping performance is different between substrate types
# this is in the supplementary materials



#-------------------------------------------------------------------------------#
# Do any performance metrics differ between origin/raising pops?                # 
#-------------------------------------------------------------------------------#

#-----------------------------------------------
# Loading Data (filename = "performance_all.csv")
data <- read.csv(file="performance_all.csv", stringsAsFactors = FALSE)
str(data)

#LOG THE RESPONSE VARIABLES
data$Lsvl<-log10(data$svl)

# Are there missing values?
colSums(is.na(data)) 

# Sample Sizes
length(data$svl) #177
length(unique(data$id)) #59
length(unique(data$clutch)) #22
length(unique(data$site)) #6
table(data$urb.cat)
length(unique(data$enclo)) #6
table(data$enclo)

#sample sizes per treatment
data_nN<-data[which(data$cross.over=="nN"),]
length(unique(data_nN$id)) #14

data_nU<-data[which(data$cross.over=="nU"),]
length(unique(data_nU$id)) #16

data_uN<-data[which(data$cross.over=="uN"),]
length(unique(data_uN$id)) #15

data_uU<-data[which(data$cross.over=="uU"),]
length(unique(data_uU$id)) #14


#-------------------------------------------------------------------------------#
# Do any performance metrics differ between origin/raising pops?                # 
#-------------------------------------------------------------------------------#

#---------------------------------
# Data exploration

#A Are there missing values?
colSums(is.na(data))

#B Are there outliers?
#response variables
dotchart(data$endur,xlab="Range of Endurance",ylab="Order of the Data")
dotchart(data$sprint.ms, xlab="Range of Sprint Speed",ylab="Order of the Data")
dotchart(data$grip.c, xlab="Gripping on Concrete", ylab="Order of the Data")
dotchart(data$grip.b, xlab="Gripping on Bark", ylab="Order of the Data")
#predictor variables (continuous)
dotchart(data$endur.temp,xlab="Range of Temp (Endurance)",ylab="Order of the Data")
dotchart(data$sprint.temp,xlab="Range of Temp (Sprint)",ylab="Order of the Data")
dotchart(data$conc.temp,xlab="Range of Temp (Grip on Concrete)",ylab="Order of the Data")
dotchart(data$bark.temp,xlab="Range of Temp (Grip on Bark)",ylab="Order of the Data")
dotchart(data$order,xlab="Range of Order",ylab="Order of the Data")
#predictor variables (categorical)
table(data$cross.over)
#random effects
table(data$id)
table(data$clutch)
table(data$enclo)
table(data$site)

#C Is there collinearity?
# Yes, with SVL. 

#D Relationships Y vs. X
# We will see.

#E Spatial/temporal aspects of sampling design
# Temporal aspects NA. Spatial aspects are accounted for.

#F Interactions
#NA

#G Zero Inflation Y
sum(data$endur==0)
100*sum(data$endur==0)/nrow(data) 

sum(data$sprint.ms==0)
100*sum(data$sprint.ms==0)/nrow(data) 

sum(data$grip.c==0)
100*sum(data$grip.c==0)/nrow(data) 

sum(data$grip.b==0)
100*sum(data$grip.b==0)/nrow(data) 

#-----------------------------------------
# Make some models

## 1. Endurance
#--------------------------
library(lme4)
m1<-lmer(endur~order+endur.temp+urb.cat*treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m1)
coefs<-data.frame(coef(summary(m1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m1)
res<-data$endur-pred
plot(res~pred)

#checking normality of residuals
hist(res) 

# Interaction was not significant. Model re-run.
m1.2<-lmer(endur~order+endur.temp+urb.cat+treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m1.2)
coefs<-data.frame(coef(summary(m1.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m1.2)
res<-data$endur-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 


## 2. Sprint
#--------------------------
m2<-lmer(sprint.ms~order+sprint.temp+urb.cat*treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m2)
coefs<-data.frame(coef(summary(m2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m2)
res<-data$sprint.ms-pred
plot(res~pred)

#checking normality of residuals
hist(res) #okay 

#Interaction insignificant. Re-run without it.
m2.2<-lmer(sprint.ms~order+sprint.temp+urb.cat+treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m2.2)
coefs<-data.frame(coef(summary(m2.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m2.2)
res<-data$sprint.ms-pred
plot(res~pred) 

#checking normality of residuals
hist(res) #okay 


## 3. Gripping on Concrete
#--------------------------
m3<-lmer(grip.c~order+conc.temp+urb.cat*treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m3)
coefs<-data.frame(coef(summary(m3)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m3)
res<-data$grip.c-pred
plot(res~pred)

#checking normality of residuals
hist(res) 

# Interaction not significant. Removed and model re-run.
m3.2<-lmer(grip.c~order+conc.temp+urb.cat+treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m3.2)
coefs<-data.frame(coef(summary(m3.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m3.2)
res<-data$grip.c-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 


## 4. Gripping on Bark
#-----------------------
m4<-lmer(grip.b~order+bark.temp+urb.cat*treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m4)
coefs<-data.frame(coef(summary(m4)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m4)
res<-data$grip.b-pred
plot(res~pred) 

#checking normality of residuals
hist(res) 

# Interaction was not significant. Remove and re-run.
m4.2<-lmer(grip.b~order+bark.temp+urb.cat+treat+(1|id)+(1|clutch)+(1|enclo)+(1|site), data=data)
summary(m4.2)
coefs<-data.frame(coef(summary(m4.2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs

#checking homogeneity of variance
pred<-predict(m4)
res<-data$grip.b-pred
plot(res~pred)

#checking normality of residuals
hist(res) 

