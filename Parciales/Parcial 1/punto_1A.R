#Punto 1A
#La complejidad del algoritmo es de O(n)
matrizInferior<-function(n) {
  sample(1:20,n,replace = TRUE)
  A =matrix(c(runif(n*n, min=3, max=21)), nrow = n, ncol = n); A
  LA = A
  LA[col(LA) >= row(LA)] = 0
  suma = 0
  for(i in A){
    suma = suma + i
  }
  print(A)
  cat("Suma de los valores del triangulo inferior de la matriz es igual a: ",suma) 
}
matrizInferior(3)
matrizInferior(5)
matrizInferior(7)