Dataset <- read.csv("C:/Users/MaoHu/Dropbox/Sophomore Year 2/STA 210/Final Project/Raw Data/Dataset.csv")
View(Dataset)
attach(Dataset)
summary(Dataset)

summary(as.factor(Dataset$INCGRP))

##libraries
library(Hmisc)
library(SDMTools)
library(pROC)
library(MASS)
library(lattice)
install.packages("nnet")
library(nnet)

##intialize hosmerlem
hosmerlem = function(y, yhat, g=10) {
  cutyhat = cut(yhat, breaks = quantile(yhat, probs=seq(0, 1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}

##Insurance Variables, creating new numeric insurance variables
Dataset$H1513NUM <- as.numeric(ifelse(is.na(H1513),0,1))
Dataset$H1514NUM <- as.numeric(ifelse(is.na(H1514),0,1))
Dataset$H1515NUM <- as.numeric(ifelse(is.na(H1515),0,1))
Dataset$H1516NUM <- as.numeric(ifelse(is.na(H1516),0,1))
Dataset$H1517NUM <- as.numeric(ifelse(is.na(H1517),0,1))
Dataset$H1520NUM <- as.numeric(ifelse(is.na(H1520),0,1))
Dataset$H1521NUM <- as.numeric(ifelse(is.na(H1521),0,1))
names(Dataset)[names(Dataset)=="H1513NUM"] <- "INSUR_MEDICAID"
names(Dataset)[names(Dataset)=="H1514NUM"] <- "INSUR_VA"
names(Dataset)[names(Dataset)=="H1515NUM"] <- "INSUR_PRIVATE"
names(Dataset)[names(Dataset)=="H1516NUM"] <- "INSUR_NONE"
names(Dataset)[names(Dataset)=="H1517NUM"] <- "INSUR_OTHER"
names(Dataset)[names(Dataset)=="H1520NUM"] <- "INSUR_DONTKNOW"
names(Dataset)[names(Dataset)=="H1521NUM"] <- "INSUR_REFUSED"

names(Dataset)[names(Dataset)=="H1467"] <- "LAST_VISIT"
names(Dataset)[names(Dataset)=="H1468"] <- "VISIT_LOCATION"

##outcome variable
Dataset$VISITYEAR <- (Dataset$LAST_VISIT<4 & !is.na(Dataset$LAST_VISIT))

datasetnew <- Dataset[,c(1,2,14:35,41:51)]
data1 <- na.omit(datasetnew[,c(1:3,5:19,21:35)])

##newdataset

VISITYEAR <- as.factor(data1$VISITYEAR)
LOCALE <- as.factor(data1$LOCALE)
AGEGRP <- as.factor(data1$AGEGRP)
AGE <- as.factor(ifelse((as.numeric(AGEGRP) > 1) & (as.numeric(AGEGRP) < 5),"B",ifelse((as.numeric(AGEGRP) > 4),"C","A")))
ISBLACK <- as.factor(ifelse(data1$RACE==2,1,0))
MARITAL <-as.factor(ifelse(data1$H1132>4,5,data1$H1132))
SEX <- as.factor(data1$H1084)
URBRURAL <- as.factor(data1$URBRURAL)
CHILD <- as.factor(data1$CHILD)
HIGHSCH <- as.factor(data1$HIGHSCH)
FOODPROB <- as.factor(data1$FOODPROB)
VETERAN <- as.factor(data1$VETERAN)
INFECT <- as.factor(ifelse(data1$ACUTE_I>1,2,data1$ACUTE_I))
NONINFECT <- as.factor(ifelse(data1$ACUTE_NI>0,1,data1$ACUTE_NI))
CHRON <- as.factor(ifelse(as.numeric(data1$CHRONIC) > 2,3,data1$CHRONIC))
ADM_7Y <- as.factor(data1$ADM_7Y)
STREET <- as.factor(data1$STREET)
HOMLSS2 <- as.factor(data1$HOMLSS2)
OTRC_7 <- as.factor(data1$OTRC_7)
DRP_7 <- as.factor(data1$DRP_7)
INSUR_MEDICAID <- as.factor(data1$INSUR_MEDICAID)
INSUR_VA <- as.factor(data1$INSUR_VA)
INSUR_PRIVATE <- as.factor(data1$INSUR_PRIVATE)
INSUR_OTHER <- as.factor(data1$INSUR_OTHER)
INSUR <-  I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1)

summary(as.factor(data2$VISIT_LOCATION))

data2 <- data1[,1:2]
data2 <- cbind(data2,VISITYEAR,LOCALE,AGE,ISBLACK,MARITAL,SEX,URBRURAL,CHILD,HIGHSCH,FOODPROB,VETERAN,INFECT,NONINFECT,CHRON,ADM_7Y,STREET,HOMLSS2,OTRC_7,DRP_7,INSUR_MEDICAID,INSUR_VA,INSUR_PRIVATE,INSUR_OTHER,INSUR)

##Descriptive stats and unadjusted rates, compare with full dataset
summary(data2$SEX)/35.29
summary(as.factor(Dataset$SEX))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$SEX, mean, na.rm=T)


summary(data2$AGE)/35.29
summary(as.factor(Dataset$AGEGRP))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$AGE, mean, na.rm=T)

summary(data2$ISBLACK)/35.29
summary(as.factor(Dataset$ISBLACK))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$ISBLACK, mean, na.rm=T)

summary(data2$HIGHSCH)/35.29
summary(as.factor(Dataset$HIGHSCH))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$HIGHSCH, mean, na.rm=T)

summary(data2$INFECT)/35.29
summary(as.factor(Dataset$ACUTE_I))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$INFECT, mean, na.rm=T)

summary(data2$CHRON)/35.29
summary(as.factor(Dataset$CHRON))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$CHRON, mean, na.rm=T)

summary(data2$HOMLSS2)/35.29
summary(as.factor(Dataset$HOMLSS2))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$HOMLSS2, mean, na.rm=T)

summary(data2$STREET)/35.29
summary(as.factor(Dataset$STREET))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, as.factor(data2$STREET), mean, na.rm=T)
tapply(as.numeric(data2$VISITYEAR)-1, data2$STREET, mean, na.rm=T)

summary(data2$ADM_7Y)/35.29
summary(as.factor(Dataset$ADM_7Y))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$ADM_7Y, mean, na.rm=T)

summary(data2$DRP_7)/35.29
summary(as.factor(Dataset$DRP_7))/42.07
tapply(as.numeric(data2$VISITYEAR)-1, data2$DRP_7, mean, na.rm=T)

summary(data2$INSUR_MEDICAID)/35.29
tapply(as.numeric(data2$VISITYEAR)-1, data2$INSUR_MEDICAID, mean, na.rm=T)

summary(data2$INSUR_VA)/35.29
tapply(as.numeric(data2$VISITYEAR)-1, data2$INSUR_VA, mean, na.rm=T)

summary(data2$INSUR_PRIVATE)/35.29
tapply(as.numeric(data2$VISITYEAR)-1, data2$INSUR_PRIVATE, mean, na.rm=T)

summary(data2$INSUR_OTHER)/35.29
tapply(as.numeric(data2$VISITYEAR)-1, data2$INSUR_OTHER, mean, na.rm=T)

summary(as.numeric(data2$INSUR))
tapply(as.numeric(data2$VISITYEAR)-1, data2$INSUR, mean, na.rm=T)

##there are no rurals in data2!!
summary(as.factor(data2$URBRURAL))/35.29
summary(as.factor(Dataset$URBRURAL))

summary(as.factor(data2$CHILD))/35.29

summary(as.factor(data2$FOODPROB))/35.29

summary(as.factor(data2$MARITAL))/35.29

summary(data2$VISITYEAR)/35.29

summary(as.factor(data2$VETERAN))/35.29
summary(as.factor(data2$OTRC_7))/35.29

newreg <- glm(VISITYEAR~.,family=binomial,data=data2[,c(5:21,26)])
hosmerlem(data1$VISITYEAR, fitted(newreg), 70)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg), threshold = 0.7)
summary(newreg)

newreg3 <- glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                 STREET + HOMLSS2 + DRP_7 + INSUR_MEDICAID + INSUR_VA + INSUR_PRIVATE +INSUR_OTHER,
               family=binomial,data=data2)
hosmerlem(data1$VISITYEAR, fitted(newreg3), 70)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg3), threshold = 0.7)
anova(newreg3,newreg)
summary(newreg3)

newreg4 <- glm(VISITYEAR ~ AGE + SEX + AGE*SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                 STREET + HOMLSS2 + DRP_7 + INSUR_MEDICAID + INSUR_VA + INSUR_PRIVATE +INSUR_OTHER,
               family=binomial,data=data2)
hosmerlem(data1$VISITYEAR, fitted(newreg4), 70)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg4), threshold = 0.7)
anova(newreg4,newreg3)
summary(newreg4)


newreg5 <- glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                 STREET + HOMLSS2 + DRP_7 + INSUR,
               family=binomial,data=data2)
hosmerlem(data1$VISITYEAR, fitted(newreg5), 70)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg5), threshold = 0.7)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg5), threshold = 0.5)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg5), threshold = 0.9)
anova(newreg5,newreg3)
summary(newreg5)

newreg6 <- glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                 STREET + HOMLSS2 + DRP_7 + INSUR,
               family=binomial,weights=data2$CLIWGT,data=data2)
hosmerlem(data1$VISITYEAR, fitted(newreg6), 70)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg6), threshold = 0.7)
anova(newreg5,newreg6)
summary(newreg6)

newreg7 <- glm(VISITYEAR ~ AGE*SEX+AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                 STREET + HOMLSS2 + DRP_7 + INSUR,
               family=binomial,data=data2)
hosmerlem(data1$VISITYEAR, fitted(newreg7), 70)
confusion.matrix(data1$VISITYEAR+0, fitted(newreg7), threshold = 0.7)
anova(newreg7,newreg3)
summary(newreg7)

roc(data2$VISITYEAR,fitted(newreg7),plot=TRUE)
roc(data2$VISITYEAR,fitted(newreg5),plot=TRUE)
roc(data2$VISITYEAR,fitted(newreg4),plot=TRUE)
roc(data2$VISITYEAR,fitted(newreg3),plot=TRUE)
roc(data2$VISITYEAR,fitted(newreg),plot=TRUE)

dev.off()
par(mfcol=c(1,3),cex=1.3)
roc(data2$VISITYEAR,fitted(newreg5),plot=TRUE,main="Model (1)")
roc(data2$VISITYEAR,fitted(newreg7),plot=TRUE,main="Model (2)")
roc(data2$VISITYEAR,fitted(newreg),plot=TRUE,main="Model (3)")

roc(Dataset$VISITYEAR,runif(4207),plot=TRUE)

tapply(as.numeric(INSUR_MEDICAID),HOMLSS2,mean)

finalreg <- newreg5
summary(finalreg)
confint(finalreg)

newreg5.1 <- glm(VISITYEAR ~ SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                             STREET + HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
                           family=binomial,data=data2)
newreg5.2 <-glm(VISITYEAR ~ AGE + ISBLACK + INFECT + CHRON + ADM_7Y + 
                  STREET + HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
                family=binomial,data=data2)
newreg5.3 <- glm(VISITYEAR ~ AGE + SEX + INFECT + CHRON + ADM_7Y + 
      STREET + HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
    family=binomial,data=data2)
newreg5.4 <- glm(VISITYEAR ~ AGE + SEX + ISBLACK + CHRON + ADM_7Y + 
                   STREET + HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
                 family=binomial,data=data2)
newreg5.5 <- glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + ADM_7Y + 
      STREET + HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
    family=binomial,data=data2)
newreg5.6<-glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + 
      STREET + HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
    family=binomial,data=data2)
newreg5.7<-glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                  HOMLSS2 + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
               family=binomial,data=data2)
newreg5.8<-glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                 STREET + DRP_7 + I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
               family=binomial,data=data2)
newreg5.9<-glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
      STREET + HOMLSS2 +  I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1),
    family=binomial,data=data2)
newreg5.10<-glm(VISITYEAR ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
      STREET + HOMLSS2 + DRP_7 ,
    family=binomial,data=data2)


##Drop in deviane tests
round(anova(newreg5.1,finalreg)$Deviance,2)
round(anova(newreg5.2,finalreg)$Deviance,2)
round(anova(newreg5.3,finalreg)$Deviance,2)
round(anova(newreg5.4,finalreg)$Deviance,2)
round(anova(newreg5.5,finalreg)$Deviance,2)
round(anova(newreg5.6,finalreg)$Deviance,2)
round(anova(newreg5.7,finalreg)$Deviance,2)
round(anova(newreg5.8,finalreg)$Deviance,2)
round(anova(newreg5.9,finalreg)$Deviance,2)
round(anova(newreg5.10,finalreg)$Deviance,2)


##
as.numeric(data2$VISITYEAR)
finalregresid <- (as.numeric(data2$VISITYEAR)-1)-fitted(finalreg)
hist(finalregresid)

?pROC


#check leverage
leverage <- hatvalues(newreg5)
cooks <- cooks.distance(newreg5)

plot(leverage)
plot(cooks)

tapply(data2$INSUR,data2$HOMLSS2,mean,na.rm=T)

testreg <- multinom(data2$VISIT_LOCATION ~ AGE + SEX + ISBLACK + INFECT + CHRON + ADM_7Y + 
                      STREET + HOMLSS2 + DRP_7 + INSUR,data=data2)

dev.off()
roc(data2$VISITYEAR,fitted(newreg5),plot=TRUE,main="ROC curve for Model(1)")