#Se igualaron las funciones f(x)=g(x), la ecuacion obtenida fue k(x)=f(x)-g(x)
#La complejidad del algoritmo es de O(n)
k <- function(x) log(x+2)-sin(x)
convergencia <- function(f,x0,x1){
  iter <- 1
  cat("-----------------------------\n")
  cat(formatC( c("iteraciones","x","Error est."), width = -15, format = "f", flag = " "), "\n") 
  cat("-----------------------------\n")
  x2 <- (x0-((f(x0)*(x0-x1))/(f(x0)-f(x1))))
  e <- abs(x2-x1)
  x0 <- x1
  x1 <- x2
  datos <- c(e)
  cat(formatC( c(iter,x2,e), digits=8, width = -15, format = "f", flag = " "), "\n") 
  while(e > 1e-8){
    iter <- iter+1
    x2 <- (x0-((f(x0)*(x0-x1))/(f(x0)-f(x1))))
    e <- abs(x2-x1)
    datos <- c(datos,e)
    cat(formatC( c(iter,x2,e), digits=8, width = -15, format = "f", flag = " "), "\n") 
    x0 <- x1
    x1 <- x2
  }
  x <- seq(1, iter, by=1)
  plot(x,datos,
       pch = 19,
       main = "Iteraciones vs Error est ",
       xlab = "Iteraciones",
       ylab = "Error est",
       type = "l")
  cat("Iteraciones: ",iter,"  Resultado:",x2) 
}

convergencia(k,-1.7,-1)
