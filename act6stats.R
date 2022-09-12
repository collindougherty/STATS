#get data
attach(nyc)
pairs(~Price+Food+Decor+Service)

par(mfrow=c(1,1))
boxplot(Price~East, xlab = "East", ylab = "Price")

#the number of predictors in full model
m <- 4
#sample size
n <- length(Price)
#split the data
set.seed(3)
train.indx <- sample(1:n, n/2)

#Select training data
y.train <- nyc[train.indx,3]
x.train <- as.matrix(nyc[train.indx,4:(3+m)])

#Select the test data
y.test <- nyc[-train.indx,3]
x.test <- as.matrix(nyc[-train.indx,4:(3+m)])

#All possible subsets selection
install.packages("leaps")
library(leaps)
b <- regsubsets(x.train, y.train)
rs <- summary(b)
rs$outmat
#calculate adjusted R^2, AIC, corrected AIC, and BIC
om1 <- lm(y.train~x.train[,2])
om2 <- lm(y.train~x.train[,2]+x.train[,1])
om3 <- lm(y.train~x.train[,2]+x.train[,1]+x.train[,4])
om4 <- lm(y.train~x.train[,2]+x.train[,1]+x.train[,4]+x.train[,3])
om <- list(om1,om2,om3,om4)
n.train <- length(y.train)
#number of parameters in each model in "om"
npar <- 3:(2+m)
#adjusted R^2
Rsq.adj <- round(rs$adjr2,3)
Rsq.adj
#AIC
AIC<- sapply(1:m, function(x) round(extractAIC(om[[x]],k=2)[2],2))
AIC
#corrected AIC
AICc <- sapply(1:m, function(x) round(extractAIC(om[[x]],k=2)[2]+
                                        2*npar[x]*(npar[x]+1)/(n-npar[x]+1),2))
AICc
#BIC
BIC<- sapply(1:m, function(x) round(extractAIC(om[[x]],k=log(n))[2],2))
BIC
#all possible subsets selection with PRESS
myPRESS <- function(x,y,indx){
  m1 <- lm(y~x[,indx])
  press <- sum((m1$residuals/(1-hatvalues(m1)))^2)
  return(press)
}
PRESS.indx <- matrix("", nrow = m, ncol = m)
colnames(PRESS.indx) <- c("Food", "Decor", "Service", "East")
PRESS <- rep(0,m)
for(i in 1:m)
{
  indx <- combn(m,i)
  n.indx <- ncol(indx)
  tmp <- sapply(1:n.indx, function(m) myPRESS(x.train, y.train, indx[,m]))
  PRESS[i] <- round(min(tmp),2)
  PRESS.indx[i, indx[,which.min(tmp)]] <- "*"
}
PRESS.indx
PRESS

model1nyc = lm(Price ~ Food + Decor)
summary(model1nyc)

#codes you may need for model diagnostics.
leverage <-hatvalues(model1nyc)
#leverage points
which(leverage>4/n)
#Diagnostic plots for justification
#fitted performance plot
par(mfrow=c(2,2))
plot(Price,Food)
abline(lsfit(Price,Food),lty = 1, col=1)
plot(Price,Decor)
abline(lsfit(Price,Decor),lty = 1, col=1)
#standardized residual 
StanRes = rstandard(model1nyc)
plotStanRes <-rstandard(model1nyc)
plot(Price,StanRes, ylab="Standardized Residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)
#scale location plot
absrtsr <-sqrt(abs(StanRes))
plot(Price,absrtsr,ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(X,absrtsr),lty=2,col=1)
#normal QQ 
install.packages("ggplot2")
library(ggplot2)
qqnorm(StanRes, ylab = "Standardized Residuals")
qqline(StanRes, lty = 2, col=1)
#codes you may need for checking the distributions of X and Y.
#density estimation for 
Ypar(mfrow=c(3,2))
plot(density(Price,bw="SJ",kern="gaussian"),type="l",main="Gaussian kernel density estimate",xlab="Price")
rug(Price)

#predicted value for the test data
yPred <- sapply(1:length(y.test), function(m) sum(om3$coefficients*c(1,x.test[m,c(2,1,4)])))
plot(y.test, yPred, main="Predicted Price vs Observed Price", xlab ="observed Price in test data",
     ylab = "predicted Price")
#MSPE
MSPE <- mean((y.test-yPred)^2)
MSPE
