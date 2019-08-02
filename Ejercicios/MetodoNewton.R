newtonraphson = function(fun, x0, tol = 0.000000005, maxiter = 100){ 
  # f = string
  numiter = 0
  g   = parse(text=fun)       # parse devuelve tipo "expression"
  g.  = D(g,"x")
  fx  = function(x){eval(g)}  # convertir f  a función
  fp  = function(x){eval(g.)} # convertir f’ a función
  correccion = -fx(x0)/fp(x0)
  while (abs(correccion) >= tol && numiter <= maxiter) {
    numiter = numiter + 1
    if (fp(x0) == 0) stop("División por cero")
    x1 = x0 + correccion
    correccion = -fx(x1)/fp(x1)
    x0 = x1 }
  if (numiter > maxiter){ warning("Se alcanzó el máximo número de iteraciones.") cat("Estado:\n")
    cat("k = ", k, "x = ", x1, " f(x) = ", f(x1), "Error estimado <= ", correccion)
  } else {
    return(list(cero = x0, f.cero = fx(x0), numeroiter=numiter, error.est = correccion))
  } 
  }
# recibe la función como una tira
newtonraphson("(x-1.8974)^3", 0, 0.00000005, 50)
a <- expression(x+10)
solve(a)
