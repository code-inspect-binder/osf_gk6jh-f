################################################################################################################################################
# 
# Paper Title: Shrinking into the big city - Influences of genetic and environmental factors on urban dragon lizard morphology and performance
# Authors: James Baxter-Gilbert, Julia Riley, Celine Frere, Martin Whiting
# Script Description: Code for Figure 2
#
# Reviewed 20 April 2020
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#HATCHLING SVL DIFFERENCES

# LOAD DATA (filename = "morphology_all.csv")
hatch_data <- read.csv(file="morphology_all.csv", stringsAsFactors = FALSE)
str(hatch_data)
hatch_data$urb_cat<-as.factor(hatch_data$urb_cat)
# Subset Data to Include Hatchlings Only
hatch_data<-hatch_data[which(hatch_data$dataset=="hatchling"),]
str(hatch_data)
#LOG THE RESPONSE VARIABLES
hatch_data$Lsvl<-log10(hatch_data$svl)
hatch_data$Lforearm<-log10(hatch_data$forearm)
hatch_data$Lhindlimb<-log10(hatch_data$hindlimb)
hatch_data$Lfoot<-log10(hatch_data$foot)
#MODEL
library(lme4)
m1<-lmer(Lsvl~urb_cat+(1|origin_site)+(1|mom_id), data=hatch_data)
summary(m1)
coefs<-data.frame(coef(summary(m1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs
#MAKE PREDICTIONS
MyData<-expand.grid(urb_cat = levels(hatch_data$urb_cat))
X<-model.matrix(~urb_cat, data=MyData)
betas<-fixef(m1)
MyData$pred<-X%*%betas
covbetas<-vcov(m1)
MyData$se<-sqrt(diag(X%*%covbetas%*%t(X)))
MyData$up<-MyData$pred+1.96*MyData$se
MyData$lo<-MyData$pred-1.96*MyData$se
head(MyData)
#MAKE A NEW DATASET
blank = c(1,2)
species = c("Natural", "Urban")
lower = c(MyData$lo)
upper = c(MyData$up)
pred = c(MyData$pred)
data = data.frame(blank, species, pred, lower, upper)
data

#Compare graphically
layout(matrix(c(1:2),nrow=1))
#par(cex = 1.5)
par(mar = c(1, 2, 1, 2), oma = c(1, 4, 1, 1))

plot(pred~blank, data, ylim=c(1.60,1.70), xlim=c(0.5,2.5), las=1, xaxt="n", ann = FALSE, type='n')
library(Hmisc)
errbar(data$blank, data$pred, data$lower, data$upper, cap=0.15, lwd=2.5, main=NULL, sub=NULL, xlab=NULL, ylab=NULL, add=TRUE)
points(data$pred~data$blank, cex = 2, pch=21, lwd=2, col = "black", bg = c("green","grey"))
mtext("Natural", side = 1, line = 0.5, adj = 0.25, font = 2)
mtext("Urban", side = 1, line = 0.5, adj = 0.75, font = 2)
mtext("Log-Transformed Snout-vent Length (mm)", side = 2, line = 3, font = 2)
mtext("(A)", font = 2, side = 3, line = -1.25, adj = 0.02)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ADULT MALE SVL DIFFERENCES

# LOAD DATA (filename = "morphology_all.csv")
data <- read.csv(file="morphology_all.csv", stringsAsFactors = FALSE)
str(data)
# Subset Data to Include Adults Only
data<-data[which(data$dataset=="adult"),]
str(data)
#subset for MALES ONLY
data_AM<-data[which(data$sex=="M"),]
str(data_AM)
data_AM$urb_cat<-as.factor(data_AM$urb_cat)
data_AM$Lsvl<-log10(data_AM$svl)
data_AM$Lforearm<-log10(data_AM$forearm)
data_AM$Lhindlimb<-log10(data_AM$hindlimb)
data_AM$Lfoot<-log10(data_AM$foot)
#MODEL
m2<-lmer(Lsvl~urb_cat+(1|origin_site), data=data_AM)
summary(m2)
coefs<-data.frame(coef(summary(m2)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs$p.z<-round(coefs$p.z, digits=4)
coefs
#MAKE PREDICTIONS
MyData<-expand.grid(urb_cat = levels(data_AM$urb_cat))
X<-model.matrix(~urb_cat, data=MyData)
betas<-fixef(m2)
MyData$pred<-X%*%betas
covbetas<-vcov(m2)
MyData$se<-sqrt(diag(X%*%covbetas%*%t(X)))
MyData$up<-MyData$pred+1.96*MyData$se
MyData$lo<-MyData$pred-1.96*MyData$se
head(MyData)
#MAKE A NEW DATASET
blank = c(1,2)
species = c("Natural", "Urban")
lower = c(MyData$lo)
upper = c(MyData$up)
pred = c(MyData$pred)
data = data.frame(blank, species, pred, lower, upper)
data

#PLOT
plot(pred~blank, data, ylim=c(2.30,2.40), xlim=c(0.5,2.5), las=1, xaxt="n", ann = FALSE, type='n')
library(Hmisc)
errbar(data$blank, data$pred, data$lower, data$upper, cap=0.15, lwd=2.5, main=NULL, sub=NULL, xlab=NULL, ylab=NULL, add=TRUE)
points(data$pred~data$blank, cex = 2, pch=21, lwd=2, col = "black", bg = c("green","grey"))
mtext("Natural", side = 1, line = 0.5, adj = 0.25, font = 2)
mtext("Urban", side = 1, line = 0.5, adj = 0.75, font = 2)
#mtext("Snout-vent-length (mm)", side = 2, line = 3, font = 2)
mtext("(B)", font = 2, side = 3, line = -1.25, adj = 0.02)

