#QUESTION 1
x = (1:600)/100 ### the values of x that we want to calculate CDF and PDF

x.cdf = pgeom(x,prob = .1)  
x.pdf = dgeom(x,prob = .1)  

plot(x,x.cdf,type='l')

plot(x,x.pdf,type='l',main='PDF of Unif(1,5)')

set.seed(3.14*09608168)
n = 500
y = rgeom(n,prob=.1)
y

y_ecdf = ecdf(y) ### this is a function
y
x.y_ecdf = y_ecdf(x)

plot(x,x.y_ecdf,type = "l") 


#QUESTION 2
set.seed(3.14*09608168)
n = 500
y = rexp(n,rate=2)

y.mean = mean(y)
y.mean
plot(y)
y

#QUESTION 3
set.seed(3.14*09608168)
n = 500
y = rexp(n,rate=2)

y.mean = mean(y)
EY = y.mean
C = (500+(100*y^2)-25*y)
mean(C) #EC
