x <- c(0.08, 0.03, 0.05, 0.2)
y <- c(0.05, 0.1, 0.2, 0.2)

grid.newpage()
grid.bezier(x, y)
x <- c(0.08, 0.17, 0.28, 0.25)
y <- c(0.05, 0.0, 0.05, 0.6)
grid.bezier(x, y)
grid.segments(.25, .6, .5, .06)
x <- c(0.5, 0.44, 0.46, 0.59)
y <- c(0.06, 0.25, 0.55, 0.6)
grid.bezier(x, y)
x <- c(0.59, 0.7, 0.74, 0.54)
y <- c(0.6, 0.64, 0.48, 0.45)
grid.bezier(x, y)

