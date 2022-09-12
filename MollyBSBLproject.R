hitters<-Hitters[complete.cases(Hitters),]

par(mfrow=c(1,3))
boxplot(hitters$Salary~hitters$NewLeague, xlab = "New League", ylab = "Salary")
boxplot(hitters$Salary~hitters$League, xlab = "League", ylab = "Salary")
boxplot(hitters$Salary~hitters$Division, xlab = "Division", ylab = "Salary")


hitters$League[which(hitters$League=="N")] <- 1
hitters$League[which(hitters$League=="A")] <- 0
hitters$League <- as.numeric(hitters$League)

hitters$Division[which(hitters$Division=="W")] <- 1
hitters$Division[which(hitters$Division=="E")] <- 0
hitters$Division <- as.numeric(hitters$Division)

hitters$NewLeague[which(hitters$NewLeague=="N")] <- 1
hitters$NewLeague[which(hitters$NewLeague=="A")] <- 0
hitters$NewLeague <- as.numeric(hitters$NewLeague)

#loess(Salary ~ Walks+Division+PutOuts+League, data=hitters)


mfull<- lm('Salary~.', data=hitters)
summary(mfull)

#data vis
StanRes <- rstandard(mfull)
absrtsr <- sqrt(abs(StanRes))
par(mfrow=c(2,2))
plot(mfull$fitted, hitters$Salary, xlab="Fitted salary values", ylab="Observed salary values")
abline(a=0,b=1,lty = 1, col=1)
plot(mfull$fitted,StanRes, xlab="Fitted salary values", ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(mfull$fitted,absrtsr,xlab = "Fitted Salary values", 
     ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(mfull$fitted,absrtsr),lty=2,col=1)
qqnorm(StanRes, ylab = "Standardized Residuals")
qqline(StanRes, lty = 2, col=1)


#the number of predictors in full model
m <- 19
#sample size
n <- length(hitters$Salary)
#split the data
set.seed(3)
X <- hitters[-c(19)]
train.indx <- sample(1:n, 4*n/5)
#Select training data
y.train <- hitters[train.indx,19]
x.train <- as.matrix(X[train.indx,])
#Select the test data
y.test <- hitters[-train.indx,19]
x.test <- as.matrix(X[-train.indx,])
comb.train<-as.data.frame(cbind(x.train,y.train))

#All possible subsets selection
library("leaps")
b <- regsubsets(y.train~x.train,comb.train,nvmax = 19)
all_poss_summ <- summary(b)


#calculate adjusted R^2, AIC, corrected AIC, and BIC
n.train <- length(y.train)
#number of parameters in each model in "om"
npar <- m+2
#adjusted R^2
rs <- summary(b)
Rsq.adj <- round(rs$adjr2,3)
Rsq.adj
idxRsqadj <- which.max(all_poss_summ$adjr2)
idxRsqadj
all_poss_summ$which[idxRsqadj,]
#AIC
min_idxAIC <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1))
min_idxAIC
AIC <- all_poss_summ$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
#corrected AIC
min_idxAICc <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
AICc<- all_poss_summ$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
all_poss_summ$which[min_idxAICc,]
#BIC
min_idxBIC <- which.min(all_poss_summ$bic)
min_idxBIC
BIC <- all_poss_summ$bic[min_idxBIC]
all_poss_summ$which[min_idxBIC,]


#Rsquared adj says we should use 12 variables. almost certainly too many
mreduced <- lm('Salary~AtBat+Hits+Runs+Walks+Years+CHmRun+CRuns+CWalks+Division+PutOuts+Errors', data=hitters)
summary(mreduced)
vif(mreduced)
mmp(mreduced,ask=FALSE)
anova(mreduced, mfull)
avPlots(mreduced)

#AIC, AICC, BIC says we should use 8 variables
pairs( ~ hitters$Salary+hitters$AtBat+hitters$Hits+hitters$Walks+hitters$CHits+hitters$CHmRun+hitters$CRuns+hitters$CWalks+hitters$Division+hitters$PutOuts)
mpartial <- lm('Salary ~ AtBat+Hits+Walks+CHits+CHmRun+CRuns+CWalks+Division+PutOuts', data=hitters)
summary(mpartial)
vif(mpartial)
mmp(mpartial,ask=FALSE)
anova(mpartial, mfull)
avPlots(mpartial)

#the BIC model is decent. some variance inflation. not all coeffs significant

##let's redo the 8var model with transformations
library("car")
bxCx <- powerTransform(cbind(hitters$AtBat,hitters$Hits,hitters$CRBI,hitters$CRuns,hitters$CWalks,hitters$CHits,hitters$CAtBat,hitters$Years,(hitters$HmRun+1),(hitters$CHmRun+1),(hitters$Walks+1),(hitters$PutOuts+1),(hitters$Assists+1), (hitters$Errors+1),hitters$Salary))
summary(bxCx)


library("car")
bxCx <- powerTransform(cbind(hitters$AtBat,hitters$Hits,(hitters$Runs+1),(hitters$RBI+1),hitters$CRBI,hitters$CRuns,hitters$CWalks,hitters$CHits,hitters$CAtBat,hitters$Years,(hitters$HmRun+1),(hitters$CHmRun+1),(hitters$Walks+1),(hitters$PutOuts+1),(hitters$Assists+1), (hitters$Errors+1),hitters$Salary))
summary(bxCx)



CAvg <- hitters$CHits/hitters$CAtBat
COBP <- (hitters$CHits + hitters$CWalks)/hitters$CAtBat
#CHRperAB <- hitters$CHmRun/hitters$CAtBat
AByr <- hitters$CAtBat/hitters$Years
POyr <- hitters$PutOuts/hitters$Years
Ayr <- hitters$Assists/hitters$Years
RAB <- hitters$CRuns/hitters$CAtBat
RBIAB <- hitters$CRBI/hitters$CAtBat
BBAB <- hitters$CWalks/hitters$CAtBat
CEAB <- hitters$Errors/hitters$CAtBat
Hseas <- hitters$Hits/hitters$CHits
Rseas <- hitters$Runs/hitters$CRuns



Salary <- hitters$Salary

#apply box cox recommendations
hitters$AtBat <- (hitters$AtBat)^(0.5)
hitters$Hits <- (hitters$Hits)^(0.5)
hitters$Runs <- (hitters$Runs)^(0.33)
hitters$RBI <- (hitters$RBI)^(0.33)
hitters$CRBI <- (hitters$CRBI)^(0.2)
hitters$CRuns <- (hitters$CRuns)^(0.17)
hitters$CWalks <- (hitters$CWalks)^(0.19)
hitters$CHits <- (hitters$CHits)^(0.2)
hitters$CAtBat <- (hitters$CAtBat)^(0.24)
hitters$Years <- (hitters$Years)^(0.5)
hitters$HmRun <- (hitters$HmRun)^(0.33)
hitters$CHmRun <- (hitters$CHmRun)^(0.16)
hitters$Walks <- (hitters$Walks)^(0.33)
hitters$PutOuts <- (hitters$PutOuts)^(0.33)
hitters$Assists <- (hitters$Assists)^(0.08)
hitters$Errors <- (hitters$Errors)^(0.33)
hitters$Salary <- log(hitters$Salary)
lsalary <- hitters$Salary

#fix dataframe so we can work with it
for(i in 1:length(colnames(hitters))){
  hitters[,i] <- as.numeric(hitters[,i])
}


#the number of predictors in full model
m=19
n=length(hitters$Salary)
#split the data
set.seed(-17)
X <- hitters[-c(19)]
train.indx <- sample(1:n, n/2)
#Select training data
y.train <- lsalary[train.indx]
x.train <- as.matrix(X[train.indx,])
#Select the test data
y.test <- lsalary[-train.indx]
x.test <- as.matrix(X[-train.indx,])
comb.train<-as.data.frame(cbind(x.train,y.train))
comb.test<-as.data.frame(cbind(x.test,y.test))

#All possible subsets selection
library("leaps")
b <- regsubsets(y.train~x.train,comb.train,nvmax = 19)
all_poss_summ <- summary(b)


#calculate adjusted R^2, AIC, corrected AIC, and BIC
n.train <- length(y.train)
#number of parameters in each model in "om"
npar <- m+2
#adjusted R^2
Rsq.adj <- round(all_poss_summ$adjr2,3)
Rsq.adj
idxRsqadj <- which.max(all_poss_summ$adjr2)
idxRsqadj
all_poss_summ$which[idxRsqadj,]
#AIC
min_idxAIC <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1))
min_idxAIC
AIC <- all_poss_summ$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
#corrected AIC
min_idxAICc <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
AICc<- all_poss_summ$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
all_poss_summ$which[min_idxAICc,]
#BIC
min_idxBIC <- which.min(all_poss_summ$bic)
min_idxBIC
BIC <- all_poss_summ$bic[min_idxBIC]
all_poss_summ$which[min_idxBIC,]


#Rsquared adj says we should use 10 variables

#AIC, AICC, BIC says we should use 2 variables when transformed
all_poss_summ$which[min_idxBIC,]
mtrans <- lm(lsalary~hitters$CRuns+hitters$PutOuts)
summary(mtrans)
vif(mtrans)
par(mfrow=c(1,1))
mmp(mtrans,ask=FALSE)
#isn't that good

## ***********
#with 4..
all_poss_summ$which[4,]
mtrans <- lm(lSalary~hitters$Hits+ hitters$RBI+hitters$PutOuts+hitters$CRBI)
summary(mtrans)
vif(mtrans)
par(mfrow=c(1,1))
mmp(mtrans,ask=FALSE)

#still not great. but in terms of diagnostics, 
#this may be as good as we get without using auxiliary variables
StanRes <- rstandard(mtrans)
absrtsr <- sqrt(abs(StanRes))
par(mfrow=c(1,1))
plot(mtrans$fitted, lSalary, xlab=paste("fitted ", expression(log(Salary)),sep=""))
abline(a=0,b=1,lty = 1, col=1)
plot(mtrans$fitted,absrtsr,xlab = paste("fitted ", expression(log(Salary)),sep=""), 
     ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(mtrans$fitted,absrtsr),lty=2,col=1)
qqnorm(StanRes, ylab = "Standardized Residuals")
qqline(StanRes, lty = 2, col=1)

#is pretty good. maybe some non-normality, but it fits the data for the most part
#not much multicoll

#testing
##now with test data
b <- regsubsets(y.test~x.test,comb.test,nvmax = 19)
all_poss_summ <- summary(b)

n.test <- length(y.test)
npar <- m+2
#adjusted R^2
Rsq.adj <- round(all_poss_summ$adjr2,3)
Rsq.adj
idxRsqadj <- which.max(all_poss_summ$adjr2)
idxRsqadj
all_poss_summ$which[idxRsqadj,]

#AIC
AIC <- all_poss_summ$bic+ (2 - log(n))*(npar+1)
AIC
min_idxAIC <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1))
min_idxAIC
minAIC <- all_poss_summ$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
all_poss_summ$which[min_idxAIC,]

#corrected AIC
AICc <- minAICc<- all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
AICc
min_idxAICc <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
min_idxAICc
minAICc<- all_poss_summ$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
all_poss_summ$which[min_idxAICc,]

#BIC
all_poss_summ$bic
min_idxBIC <- which.min(all_poss_summ$bic)
min_idxBIC
minBIC <- all_poss_summ$bic[min_idxBIC]
all_poss_summ$which[min_idxBIC,]
#if we use the 5 variable model, we get this

mtest<-lm(lsalary~hitters$Hits+hitters$RBI+hitters$CAtBat+hitters$CRBI+hitters$Division)
summary(mtest)
##not the same variables

pairs(~lsalary+hitters$Hits+hitters$RBI+hitters$CAtBat+hitters$CRBI+hitters$Division)
vif(mtest)
mmp(mtest)

###these are okay. not the same variables, multicoll
##four var model below
all_poss_summ$which[4,]
mtest<-lm(lsalary~hitters$Hits+hitters$RBI+hitters$CAtBat+hitters$CRBI)
summary(mtest)
##not the same variables

pairs(~lsalary+hitters$Hits+hitters$RBI+hitters$CAtBat+hitters$CRBI)
vif(mtest)
mmp(mtest)

##not very good. multi coll

pairs(~CAvg+COBP+AByr+POyr+Ayr+RAB+RBIAB+BBAB+CEAB+Hseas+Rseas)

#the number of predictors in full model
m <- 14
#sample size
n <- length(lsalary)
#split the data
X <- cbind(CAvg,COBP,AByr,POyr,Ayr,RAB,RBIAB,BBAB,CEAB,Hseas,Rseas,hitters$Division,hitters$League,hitters$NewLeague)
set.seed(-17)
train.indx <- sample(1:n, n/2)
#Select training data
y.train <- lsalary[train.indx]
x.train <- as.matrix(X[train.indx,])
#Select the test data
y.test <- lsalary[-train.indx]
x.test <- as.matrix(X[-train.indx,])
comb.train<-as.data.frame(cbind(x.train,y.train))
comb.test<-as.data.frame(cbind(x.test,y.test))

#All possible subsets selection
library("leaps")
b2 <- regsubsets(y.train~x.train,comb.train,nvmax = 14)
all_poss_summ <- summary(b2)

n.train <- length(y.train)
#number of parameters in each model in "om"
npar <- m+2
#adjusted R^2
Rsq.adj <- round(all_poss_summ$adjr2,3)
Rsq.adj
idxRsqadj <- which.max(all_poss_summ$adjr2)
idxRsqadj
all_poss_summ$which[idxRsqadj,]
#AIC
min_idxAIC <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1))
min_idxAIC
all_poss_summ$bic+ (2 - log(n))*(npar+1)
AIC <- all_poss_summ$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
all_poss_summ$which[min_idxAIC,]
#corrected AIC
min_idxAICc <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
AICc<- all_poss_summ$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
all_poss_summ$which[min_idxAICc,]
#BIC
min_idxBIC <- which.min(all_poss_summ$bic)
min_idxBIC
all_poss_summ$bic
BIC <- all_poss_summ$bic[min_idxBIC]
all_poss_summ$which[min_idxBIC,]

#should probably do the 5 variable if using auxiliary

pairs(~lsalary + CAvg +AByr+ CEAB+RBIAB+Rseas)
modelAux1 <- lm(lsalary ~ CAvg +CEAB+ AByr+RBIAB+Rseas)

library("car")
summary(modelAux1)
vif(modelAux1)
par(mfrow=c(1,1))
mmp(modelAux1,ask=FALSE)
avPlots(modelAux1)

bc<-powerTransform(cbind(CAvg,AByr,RBIAB,(Rseas+1),Salary))
summary(bc)

par(mfrow=c(2,2))
StanRes <- rstandard(modelAux1)
absrtsr <- sqrt(abs(StanRes))
plot(modelAux1$fitted, lsalary, xlab=paste("fitted ", expression(log(Salary)),sep=""))
abline(a=0,b=1,lty = 1, col=1)
plot(modelAux1$fitted,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(modelAux1$fitted,absrtsr,xlab = paste("fitted ", expression(log(Salary)),sep=""), 
     ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(modelAux1$fitted,absrtsr),lty=2,col=1)
qqnorm(StanRes, ylab = "Standardized Residuals")
qqline(StanRes, lty = 2, col=1)

modelFalse<- lm(Salary~CAvg +AByr+ CEAB+RBIAB+Rseas)
par(mfrow=c(1,1))
plot(modelFalse$fitted, Salary, xlab="fitted Salary values")
abline(a=0,b=1,lty = 1, col=1)

#is pretty good. maybe some non-normality, but it fits the data for the most part
#not much multicoll


##MODEL VALIDATION
##now with test data
b <- regsubsets(y.test~x.test,comb.test,nvmax = 14)
all_poss_summ <- summary(b)

n.test <- length(y.test)
npar <- m+2
#adjusted R^2
Rsq.adj <- round(all_poss_summ$adjr2,3)
Rsq.adj
idxRsqadj <- which.max(all_poss_summ$adjr2)
idxRsqadj
all_poss_summ$which[idxRsqadj,]

#AIC
AIC <- all_poss_summ$bic+ (2 - log(n))*(npar+1)
AIC
min_idxAIC <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1))
min_idxAIC
minAIC <- all_poss_summ$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
all_poss_summ$which[min_idxAIC,]

#corrected AIC
AICc <- minAICc<- all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
AICc
min_idxAICc <- which.min(all_poss_summ$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
min_idxAICc
minAICc<- all_poss_summ$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
all_poss_summ$which[min_idxAICc,]

#BIC
all_poss_summ$bic
min_idxBIC <- which.min(all_poss_summ$bic)
min_idxBIC
minBIC <- all_poss_summ$bic[min_idxBIC]
all_poss_summ$which[min_idxBIC,]
all_poss_summ$which[5,]
#if we use the 5 variable model, we get the same as in training data
#if we use the 6 variable, here's what we get
mtest<-lm(lsalary~CAvg+AByr+RAB+RBIAB+CEAB+Rseas)
summary(mtest)


pairs(~lsalary+CAvg+AByr+RAB+RBIAB+CEAB+Rseas)
vif(mtest)
mmp(mtest)

StanRes <- rstandard(mtest)
absrtsr <- sqrt(abs(StanRes))
par(mfrow=c(2,2))
plot(mtest$fitted, lsalary, xlab=paste("fitted ", expression(log(Salary)),sep=""))
abline(a=0,b=1,lty = 1, col=1)
plot(mtest$fitted,absrtsr,xlab = paste("fitted ", expression(log(Salary)),sep=""), 
     ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(mtest$fitted,absrtsr),lty=2,col=1)
qqnorm(StanRes, ylab = "Standardized Residuals")
qqline(StanRes, lty = 2, col=1)

par(mfrow=c(4,2))
plot(mtest$fitted,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(Rseas,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(RBIAB,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(AByr,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(CAvg,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(RAB,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(CEAB,StanRes, ylab="Standardized Residuals") 
abline(h=2,lty=2)
abline(h=-2,lty=2)

yPred <- sapply(1:length(y.test), function(m) sum(modelAux1$coefficients*c(1,x.test[m,c(1,9,3,7,11)])))
plot(y.test, yPred, main="Predicted log(Salary) vs Observed log(Salary)", xlab ="observed log(Salary) in test data",
     ylab = "predicted log(Salary)")

mse <- function(model) 
  mean(model$residuals^2)
mse(modelAux1)
#MSPR
MSPR1 <- mean((y.test-yPred)^2)
MSPR1
