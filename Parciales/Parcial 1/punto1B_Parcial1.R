#Punto 1-b
#La complejidad de el Algoritmo es de O(n)
sumaMatrix <- function(n){
  x <- sample(1:10,n*n,replace=T)
  A <- matrix( 
    c(x), # the data elements 
    nrow=n,              # number of rows 
    ncol=n,              # number of columns 
    byrow = TRUE)
  suma = 0
  A
  for(i in A){
    suma = suma + i
  }
  print(A)
  cat("Suma de los valores de la matriz es igual a: ",suma) 
}
sumaMatrix(4)
sumaMatrix(6)
sumaMatrix(8)
sumaMatrix(5)
sumaMatrix(2)
