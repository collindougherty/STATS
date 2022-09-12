#part 1
rm(list=ls())
n = 200
lambda = 4
sigma = sqrt(lambda) #since a poisson distribution has standard deviation of sqrt(lambda)
#rpois() is the R function to genearte random sample from Poisson distribution.
x = rpois(n, lambda)
print(x)

#part 2
hist(x)
mean(x)
sd(x)
# the mean is close to the actual mean of lambda = 4, the standard deviation is close to the actual 
# standard deviation of 2

#part 3
#confidence interval for x
left = (mean(x) - qnorm(.975)*(sqrt(lambda)/sqrt(n)))
right = (mean(x) + qnorm(.975)*(sqrt(lambda)/sqrt(n)))
print(left)
print(right)
# yes, the actual mean lambda is in the confidence interval

#question 4
x.bar <- rep(NA, 1000)
for(i in 1:1000)
{
  y = rpois(n, lambda)
x.bar[i] <- mean(y)
}
print(x.bar)

lowerbound <- rep(NA, 1000) 
for(i in 1:1000)
{
  lowerbound[i] <- x.bar[i] - qnorm(.975)*(2/sqrt(200))
}

upperbound <- rep(NA, 1000) 
for(i in 1:1000)
{
  upperbound[i] <- x.bar[i] + qnorm(.975)*(2/sqrt(200))
}

counter <- 0
for(i in 1:1000)
{
  if((lowerbound[i]<=4)&(upperbound[i]>4))
    counter <- counter + 1
}
print(counter)

#part 5
x.bar2 <- rep(NA, 400)
for(i in 1:400)
{z <- rpois(n, lambda)
x.bar2[i] <- mean(z)
}

print(x.bar2)
mean(x.bar2)
hist(x.bar2)
#it is approximately normal cenetered on 4.0