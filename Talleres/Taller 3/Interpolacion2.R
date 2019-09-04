p<-c(15,13,21,27,35,30)

library(Matrix)
library(PolynomF)

## ################################################## 
## ##                                              ##
## ##               Interpolacion 1                ##
## ##                                              ##
## ##################################################

f<-function(x)1/(1+x^2)
x<-seq(-2,2,1)
y<-f(x)
plot(x,y)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=1, col="orange" ,asp=1,xlab="X", ylab="Y", main="mano Derecha ")
curve(ajuste_polinomio, add=T, from = -2, to=2, xlim = c(-9,9), ylim=c(-9,9))

par(new=TRUE)

f<-function(x)1/(1+x^2)
x<-seq(-2,2,4/6)
y<-f(x)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=0.8, col="black" ,asp=1,xlab="X", ylab="Y", main="mano Derecha ")
curve(ajuste_polinomio, add=T, from = -2, to=2, xlim = c(-9,9), ylim=c(-9,9))

par(new=TRUE)

f<-function(x)1/(1+x^2)
x<-seq(-2,2,4/10)
y<-f(x)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=0.6, col="red" ,asp=1,xlab="X", ylab="Y", main="mano Derecha ")
curve(ajuste_polinomio, add=T, from = -2, to=2, xlim = c(-9,9), ylim=c(-9,9))

par(new=TRUE)

f<-function(x)1/(1+x^2)
x<-seq(-2,2,4/15)
y<-f(x)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=0.4, col="blue" ,asp=1,xlab="X", ylab="Y", main="mano Derecha ")
curve(ajuste_polinomio, add=T, from = -2, to=2, xlim = c(-9,9), ylim=c(-9,9))

## ################################################## 
## ##                                              ##
## ##               Interpolacion 2                ##
## ##                                              ##
## ##################################################


f<-function(x)1/(1+x^2)
i<-seq(1,6)
x<-cos(((2*i+1)/(2*6+2))*pi)+5
y<-f(x)
plot(x,y)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=1, col="black" ,asp=1,xlab="X", ylab="Y", main="Interpolacion ")
curve(ajuste_polinomio, add=T, from = -6, to=6, xlim = c(-10,10), ylim=c(-10,10))

par(new=TRUE)

f<-function(x)1/(1+x^2)
i<-seq(1,10)
x<-cos(((2*i+1)/(2*10+2))*pi)+5
y<-f(x)
plot(x,y)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=1, col="red" ,asp=1,xlab="X", ylab="Y", main="Interpolacion")
curve(ajuste_polinomio, add=T, from = -6, to=6, xlim = c(-10,10), ylim=c(-10,10))

par(new=TRUE)

f<-function(x)1/(1+x^2)
i<-seq(1,15)
x<-cos(((2*i+1)/(2*15+2))*pi)+5
y<-f(x)
plot(x,y)
ajuste_polinomio = poly_calc(x,y,1e-10)
points(x,y, pch=19, cex=1, col="blue" ,asp=1,xlab="X", ylab="Y", main="Interpolacion ")
curve(ajuste_polinomio, add=T, from = -6, to=6, xlim = c(-10,10), ylim=c(-10,10))


## ################################################## 
## ##                                              ##
## ##               Interpolacion 3                ##
## ##                                              ##
## ##################################################

