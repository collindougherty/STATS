2+3

4*5/2

7^2

sqrt(2)

exp(1)

pi

xx <- 7*2/pi

xx

xx^2

xx = 7*2/pi

x = c(7,3,8,12)

x^2

seq(from =1, to = 5)

1:5

seq(from = 2, to = 1, length.out = 5)

x

x[1]

x[2:4]

x[c(1,3,4)]

x[-c(1,3)]

sum(x)

length(x)

mean(x)

var(x)

sd(x)

sum(x[2:4])

length(x[2:4])

xmean <- mean(x)

xmean

rm(list=ls())

getwd()

myfavoritefunction <- function(x){
  output=x+x^2
  return(output)
}

x1= c(-1,0,1,2)

myfavoritefunction(x1)

y1 = myfavoritefunction(x1)
y1

x = 1:100
y <- myfavoritefunction(x)

plot(x,y,type = 'p')

plot(x,y,type='l')

set.seed(08212019)

n = 3

x1 = rexp(n,rate=2)
x1

x2 = rexp(n,rate=2)
x2

set.seed(08212019)
x3 = rexp(n,rate=2)
x3

set.seed(08212019)
n = 100
x = rexp(n,rate=2)

y = 100+10*x+rnorm(n,mean=0,sd=1)

plot(x,y,type = 'p')

hist(x,breaks = 10)

hist(y,breaks= 10)