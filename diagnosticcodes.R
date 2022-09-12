#codes you may need for model diagnostics.
leverage <-hatvalues(pgalm)
#leverage points
which(leverage>4/n)
#Diagnostic plots for justification
#fitted performance plot
par(mfrow=c(2,2))
plot(Weekday,Sunday)
abline(lsfit(Weekday,Sunday),lty = 1, col=1)
#standardized residual 
StanRes = rstandard(pgalm)
plotStanRes <-rstandard(pgalm)
plot(PrizeMoney,StanRes, ylab="Standardized Residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)
#scale location plot
absrtsr <-sqrt(abs(StanRes))
plot(PrizeMoney,absrtsr,ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(PrizeMoney,absrtsr),lty=2,col=1)
#normal QQ 
qqnorm(StanRes, ylab = "Standardized Residuals")
qqline(StanRes, lty = 2, col=1)
#codes you may need for checking the distributions of X and Y.
#density estimation for 
Ypar(mfrow=c(3,2))
plot(density(Y,bw="SJ",kern="gaussian"),type="l",main="Gaussian kernel density estimate",xlab="Y")1
rug(Y)
#boxplot of 
Yboxplot(Y,ylab="Y")
#Normal QQ plot for 
Yqqnorm(Y, ylab = "Y")
qqline(Y, lty = 2, col=2)
#density estimation for 
Xplot(density(X,bw="SJ",kern="gaussian"),type="l",main="Gaussian kernel density estimate",xlab="X")
rug(X)
#boxplot of 
Xboxplot(X,ylab="X")
#Normal QQ plot for 
Xqqnorm(X, ylab = "X")
qqline(X, lty = 2, col=2)
