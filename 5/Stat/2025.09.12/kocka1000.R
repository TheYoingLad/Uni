data <- floor(runif(1000, 1, 7))

plot(ecdf(data), main="Relatív gyakoriság 1000 dobásnál", do.points=FALSE)