library(Matrix)
library(PolynomF)

x=c(14.6, 14.7, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,              9.00, 9.30, 10, 10.2, 10.3, 10.0, 9.50)                                                                                                       
y=c(14.7, 14.0, 13.4, 12.3, 11.0, 10.5, 10.2, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,              13.9, 14.9, 16, 16.4, 16.8, 10.7, 11.0)     
x1=c(14.6, 14.7, 14.6, 14.8, 15.2)
y1=c(14.7, 14.0, 13.4, 12.3, 11.0)


length(x)
min(x)
max(x)


plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ", xlim = c(6,20),ylim = c(3,19))
par(new=TRUE)
plot(x1,y1, pch=13, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Diagrama ", xlim = c(6,20),ylim = c(3,19))

datosx = x[32:36]
datosy = y[32:36]

ajuste_polinomio = poly_calc(datosx,datosy,1e-10)
points(datosx,datosy, pch=19, cex=0.5, col="red" ,asp=1,xlab="X", ylab="Y", main="mano Derecha ")
curve(ajuste_polinomio, add=T, from = 11.6, to=12.9, xlim = c(6,20), ylim=c(3,19))

q=c(6,8,10,12,14,16,18,20)
w=c(7,9,12,18,21,19,15,10)

plot(q,w, pch=19, cex=1.1, col = "black", asp=1,xlab="X", ylab="Y", main="Diagrama ", xlim = c(1,20),ylim = c(1,25))

par(new = TRUE)

#Polinomio de grado 2
datX2=c(q[3],q[4],q[5]); datY2=c(w[3],w[4],w[5])
polyAjuste2 = poly_calc(datX2,datY2)
polyAjuste2
plot(datX2,datY2, pch=19, cex=1, col = "red", asp=1, axes=FALSE) 
#curve(polyAjuste2,add=T,from=0, to=21)
print(polyAjuste2)

par(new = TRUE)

#Polinomio de grado 2 desplazado 1 lugares
datX3=c(q[5],q[6],q[7]); datY3=c(w[5],w[6],w[7])
polyAjuste3 = poly_calc(datX3,datY3)
polyAjuste3
plot(datX3,datY3, pch=19, cex=0.7, col = "blue", asp=1, axes=FALSE) 
#curve(polyAjuste3,add=T,from=0, to=21)
print(polyAjuste3)

