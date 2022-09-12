rm(list = ls())
Hitters <- read.csv("C:/Users/colli/Downloads/Hitters.csv")
## there are some incomplete rows that make the model not as strong
## this next line removes those cases
hitters = Hitters[complete.cases(Hitters),]
View(hitters)

attach(hitters)

## setting variables to numeric type

League[which(League=="N")] <- 1
League[which(League=="A")] <- 0
League <- as.numeric(League)

Division[which(Division=="W")] <- 1
Division[which(Division=="E")] <- 0
Division <- as.numeric(Division)

NewLeague[which(NewLeague=="N")] <- 1
NewLeague[which(NewLeague=="A")] <- 0
NewLeague <- as.numeric(NewLeague)

#defining new variables
CAvg = CHits/CAtBat
Avg = Hits/AtBat
COBP = (CHits + CWalks)/CAtBat
OBP = (Hits + Walks)/AtBat
CHRperAB = CHmRun/CAtBat
HRperAB = HmRun/AtBat
AByr = CAtBat/Years
POyr = PutOuts/Years
Ayr = Assists/Years
CRAB = CRuns/CAtBat
RAB = Runs/AtBat
CRBIAB = CRBI/CAtBat
RBIAB = RBI/AtBat
CBBAB = CWalks/CAtBat
BBAB = Walks/AtBat
CEAB = Errors/CAtBat


PreviousYrAll.lm = lm(Salary ~ AtBat + Hits + HmRun + Runs + RBI + Walks + League + Division + NewLeague + Avg + OBP + HRperAB + RAB + RBIAB + BBAB)
summary(PreviousYrAll.lm)
#removed all the obvious high colinearities
PreviousYrAll.lm = lm(Salary ~ AtBat + League + Division + NewLeague + Avg + OBP + HRperAB + RAB + RBIAB + BBAB)
summary(PreviousYrAll.lm)
#removed BBAB because of perfect collinearilty
PreviousYrAll.lm = lm(Salary ~ AtBat + League + Division + NewLeague + Avg + OBP + HRperAB + RAB + RBIAB)
summary(PreviousYrAll.lm)
#model diagnostics
X = cbind(AtBat, League, Division, NewLeague, Avg, OBP, HRperAB, RAB, RBIAB)
library(leaps)
b <- regsubsets(as.matrix(X),Salary)
rs <-summary(b)
#best variables for the number of predictors
rs$outmat
#now we try and determine how many predictors we need
#number of predictors in full model
m = 15
#sample size
n <- length(hitters$Salary)
npar = m + 2
#split the data
set.seed(47)
plot(1:8,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")
plot(1:8,rs$bic,xlab="Subset Size",ylab="BIC")
plot(1:8,AIC,xlab="Subset Size",ylab="AIC")
#AIC
AIC = rs$bic+ (2 - log(n))*(npar+1)
min_idxAIC <- which.min(rs$bic+ (2 - log(n))*(npar+1))
min_idxAIC
AIC <- rs$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
#corrected AIC
min_idxAICc <- which.min(rs$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
AICc<- rs$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
AICc = rs$which[min_idxAICc,]
#BIC
min_idxBIC <- which.min(rs$bic)
min_idxBIC
BIC <- rs$bic[min_idxBIC]
#based on variable selection, we chose following model
PreviousYear.lm = lm(Salary ~ AtBat + Division + OBP)
summary(PreviousYear.lm)
#box cox
library("car")
bxCx <- powerTransform(cbind(Salary, (AtBat), (Division+1), OBP))
summary(bxCx)
#choose best model for previous year stats based on box cox
lsalary = Salary^.13
lDivision = Division^.16
lOBP = OBP^1.67
PreviousYearBest.lm = lm(lsalary ~ AtBat + lDivision + lOBP)
summary(PreviousYearBest.lm)




# Finding the best career stats model
# no league, division or new league bc that is current year only
# added Years
CareerModelAll.lm = lm(Salary ~ CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + PutOuts + Assists + Errors + Years + CAvg + COBP + CHRperAB + AByr + POyr + Ayr + CRAB + CRBIAB + CEAB)
summary(CareerModelAll.lm)
X = cbind(CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks, PutOuts, Assists, Errors, Years, CAvg, COBP, CHRperAB, AByr, POyr, Ayr, CRAB, CRBIAB, CEAB)
library(leaps)
b <- regsubsets(as.matrix(X),Salary)
rs <-summary(b)
#best variables for the number of predictors
rs$outmat
#now we try and determine how many predictors we need
#number of predictors in full model
m = 19
#sample size
n <- length(hitters$Salary)
npar = m + 2
#split the data
set.seed(47)
plot(1:8,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")
plot(1:8,rs$bic,xlab="Subset Size",ylab="BIC")
plot(1:8,AIC,xlab="Subset Size",ylab="AIC")
#AIC
AIC = rs$bic+ (2 - log(n))*(npar+1)
min_idxAIC <- which.min(rs$bic+ (2 - log(n))*(npar+1))
min_idxAIC
AIC <- rs$bic[min_idxAIC]+ (2 - log(n))*(npar+1)
#corrected AIC
min_idxAICc <- which.min(rs$bic+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)) 
AICc<- rs$bic[min_idxAICc]+ (2 - log(n))*(npar+1) +(2*npar*(npar+1))/(n-npar-1)
AICc = rs$which[min_idxAICc,]
#BIC
min_idxBIC <- which.min(rs$bic)
min_idxBIC
BIC <- rs$bic[min_idxBIC]
#based on variable selection, we chose following 8 variable model
CareerModel.lm = lm(Salary ~ CAtBat + CRuns + CRBI + PutOuts + CAvg + AByr + POyr + CEAB)
summary(CareerModel.lm)
#box cox
library("car")
bxCx <- powerTransform(cbind(Salary, CAtBat, CRuns, CRBI, (PutOuts+1), CAvg, AByr, (POyr+1), (CEAB+1)))
summary(bxCx)
#choose best model for previous year stats based on box cox
lsalary = Salary^(-.04)
lcatbat = CAtBat^.08
lcruns = CRuns^.08
lcrbi = CRBI^.06
lpo = (PutOuts+1)^.2
lcavg = CAvg^(-1)
labyr = AByr^.33
lceab = (CEAB+1)^(-105.72)
CareerModelBest.lm = lm(lsalary ~ lcatbat + lcruns + lcrbi + lcavg + lpo + labyr + lceab)
summary(CareerModelBest.lm)
#not all of these variables are statistically significant, but it is sufficient to put it forward as our best career model for comparison

#some visual displays
par(mfrow=c(1,2))
mmp(PreviousYearBest.lm,ask=FALSE)
mmp(CareerModelBest.lm,ask=FALSE)
library(car)
avPlots(PreviousYearBest.lm)
avPlots(CareerModelBest.lm)
install.packages("sjPlot")
library(sjPlot)
plot_model(CareerModelBest.lm, transform = NULL, show.values = TRUE, axis.labels = "", value.offset = .4)
plot_model(PreviousYearBest.lm, transform = NULL, show.values = TRUE, axis.labels = "", value.offset = .4)