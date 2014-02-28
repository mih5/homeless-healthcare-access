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
data1 <- na.omit(datasetnew[,c(1:4,5:19,21:35)])


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
INSUR <-  as.numeric(I(INSUR_MEDICAID==1 | INSUR_VA==1 | INSUR_PRIVATE==1 | INSUR_OTHER ==1))
VISITLOCATION <- as.factor(ifelse(as.numeric(data1$VISIT_LOCATION)<3,1,ifelse(as.numeric(data1$VISIT_LOCATION)==3,0,2)))
summary(as.factor(data1$VISIT_LOCATION))
summary(VISITLOCATION)

##some info
summary(VISITLOCATION)/35.01
summary(SEX)/35.01
summary(STREET)/35.01
summary(as.factor(INSUR))/35.01

datamulti <- data1[,1:2]
datamulti <- cbind(datamulti,VISITYEAR,LOCALE,AGE,ISBLACK,MARITAL,SEX,URBRURAL,CHILD,HIGHSCH,FOODPROB,VETERAN,INFECT,NONINFECT,CHRON,ADM_7Y,STREET,HOMLSS2,OTRC_7,DRP_7,INSUR_MEDICAID,INSUR_VA,INSUR_PRIVATE,INSUR_OTHER,INSUR,VISITLOCATION)

##I am mostly arbitrarily choosing a subset of variables on which to conduct a multinomial logistic regression.  
##I am picking a model and then removing terms to see if they change the fit.  For the LR chi-squared statistic to have chi-squared distribution, I need at least 5 cases per bin
##Saturated model
multiregs <- multinom(VISITLOCATION ~ AGE + SEX + ISBLACK + STREET + INSUR,data=datamulti)
##models without certain terms
multireg1 <- multinom(VISITLOCATION ~ SEX + ISBLACK + STREET + INSUR,data=datamulti)
multireg2 <- multinom(VISITLOCATION ~ AGE + ISBLACK + STREET + INSUR,data=datamulti)
multireg3 <- multinom(VISITLOCATION ~ AGE + SEX + STREET + INSUR,data=datamulti)
multireg4 <- multinom(VISITLOCATION ~ AGE + SEX + ISBLACK+ INSUR,data=datamulti)
multireg5 <- multinom(VISITLOCATION ~ AGE + SEX + ISBLACK + STREET,data=datamulti)
multireg0 <- multinom(VISITLOCATION ~ 1,data=datamulti)

?deviance
?multinom
##"deviance" is reported as minus twice log-likelihood.  I can use LR chi-squared test as described in agresti (and appendix by Thompson)
##the test statistic D = -2ln(maximum likelihood when parameters satisfy H0/maximum likelihood when parameters are unrestricted)
##I set H0 to be that the coefficient of the removed terms are 0.  If H0 is true, LR(X) should be small, else, it should be large
##Accordingto Agresti, D ~ chi-square (free parameters full model - free parameters small model)

##calculate D = 2ln(likelihood for multiregi)-2ln(likelihood for multiregs)
D0 <- deviance(multireg0)-deviance(multiregs)
D1 <- deviance(multireg1)-deviance(multiregs)
D2 <- deviance(multireg2)-deviance(multiregs)
D3 <- deviance(multireg3)-deviance(multiregs)
D4 <- deviance(multireg4)-deviance(multiregs)
D5 <- deviance(multireg5)-deviance(multiregs)

summary(multireg0)

anova(multireg0,multiregs)
anova(multireg1,multiregs)
1-pchisq(D0,12-0) 
1-pchisq(D1,12-8) 
1-pchisq(D2,12-8)
1-pchisq(D3,12-8)
1-pchisq(D4,12-8)
1-pchisq(D5,12-8)
##suggest removal of age, isblack

finalreg <- multinom(VISITLOCATION ~ as.factor(SEX) + as.factor(STREET)+as.factor(INSUR),data=datamulti)
anova(finalreg,multiregs)

##try out probability prediction on test model
testreg <- multinom(VISITLOCATION ~ SEX,data=datamulti)
summary(testreg)
bnot1 <- -0.3677248
bnot2 <-  1.6915140
sex1 <- -0.3108236
sex2 <- 0.2854223

##find expected
pi01 <- exp(bnot1+sex1*0)/(1+sum(exp(bnot1+bnot1*0)+exp(bnot2+bnot2*0)))
pi11 <- exp(bnot1+sex1*1)/(1+sum(exp(bnot1+bnot1*1)+exp(bnot2+bnot2*1)))
pi02 <- exp(bnot2+sex2*0)/(1+sum(exp(bnot1+bnot1*0)+exp(bnot2+bnot2*0)))
pi12 <- exp(bnot2+sex2*1)/(1+sum(exp(bnot1+bnot1*1)+exp(bnot2+bnot2*1)))
pi00 <- 1-(pi01+pi02)
pi10 <- 1-(pi11+pi12)

pi01+pi02+pi00
pi11+pi12+pi10
pi01
pi02
pi00
##if you're female/male, your predicted probabilities should add up to 1

##check with predict function

predictdata <- datamulti[2:3,]
names(predictdata)
predictdata[1,8]=1
predictdata[2,8]=2

##use type="probs"to specify want probabilities
predict(testreg,predictdata,type="probs")

##looks like above manual estimate probability and fitted probabilities are equal, excellent
##now write code to do pearson's chi-square
##from agresti: Q(x)=sum((obs-expected)^2/expected)

##create prediction matrix
##vars of interest are 8: sex, 18: street, 26: insur
predictdata2 <- datamulti[2:9,c(8,18,26)]
names(predictdata2)
predictdata2$SEX <- as.factor(predictdata2$SEX)
predictdata2$STREET <- as.factor(predictdata2$STREET)
predictdata2$INSUR <- as.factor(predictdata2$INSUR)
predictdata2[,1]=c(1,1,1,1,2,2,2,2)
predictdata2[,2]=c(0,0,1,1,0,0,1,1)
predictdata2[,3]=c(0,1,0,1,0,1,0,1)

predictdata2$SEX <- as.factor(predictdata2$SEX)
predictdata2$STREET <- as.factor(predictdata2$STREET)
predictdata2$INSUR <- as.factor(predictdata2$INSUR)
expecttable<-predict(finalreg,predictdata2,type="probs")
counts <- c(nrow(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==0)),
            nrow(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==1)),
            nrow(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==0)),
            nrow(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==1)),
            nrow(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==0)),
            nrow(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==1)),
            nrow(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==0)),
            nrow(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==1))
            )
sum(counts)
expecttable <- cbind(expecttable,counts)
expecttable
expecttable <- cbind(expecttable,expecttable[,1] *expecttable[,4],expecttable[,2] *expecttable[,4],expecttable[,3] *expecttable[,4])
observed0 <- c(nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==0),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==1),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==0),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==1),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==0),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==1),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==0),VISITLOCATION==0)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==1),VISITLOCATION==0))
               )   
observed1 <- c(nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==0),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==1),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==0),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==1),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==0),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==1),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==0),VISITLOCATION==1)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==1),VISITLOCATION==1))
)

observed2 <- c(nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==0),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==0),INSUR==1),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==0),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==1),STREET==1),INSUR==1),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==0),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==0),INSUR==1),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==0),VISITLOCATION==2)),
               nrow(subset(subset(subset(subset(datamulti,SEX==2),STREET==1),INSUR==1),VISITLOCATION==2))
)

expecttable <- cbind(expecttable,observed0,observed1,observed2)

##calculate Q(x)

q <- sum((expecttable[,5]-expecttable[,8])^2/expecttable[,5])+sum((expecttable[,6]-expecttable[,9])^2/expecttable[,6])+sum((expecttable[,7]-expecttable[,10])^2/expecttable[,7])
q
d.f. <- 24-8
p.val<-1-pchisq(q,d.f.)
p.val
##finalmodel stats
anova(multireg0,finalreg)
summary(finalreg)
round(coef(finalreg),2)
round(exp(coef(finalreg)),2)
round(exp(confint(finalreg)),2)

finalreg1 <- multinom(VISITLOCATION ~  as.factor(STREET)+as.factor(INSUR),data=datamulti)
finalreg2 <- multinom(VISITLOCATION ~ as.factor(SEX)+as.factor(INSUR),data=datamulti)
finalreg3 <- multinom(VISITLOCATION ~ as.factor(SEX) + as.factor(STREET),data=datamulti)
anova(finalreg1,finalreg)
anova(finalreg2,finalreg)
anova(finalreg3,finalreg)

subdatamulti <- subset(datamulti,VISITLOCATION=="0" | VISITLOCATION=="2")
altreg <-  multinom(VISITLOCATION ~ as.factor(SEX) + as.factor(STREET)+as.factor(INSUR),data=subdatamulti)

vb.rest <- coef(altreg)
vcov.rest <- vcov(altreg)

vb.full <- coef(finalreg)[2,]
vcov.full <- vcov(finalreg)[5:8,5:8]

db <- vb.rest-vb.full
dcov <- vcov.rest - vcov.full

stat <- t(db)%*%solve(dcov)%*%(db)
stat

abs(stat)

pchisq(abs(stat),qr(dcov)$rank,lower.tail=FALSE)