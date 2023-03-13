source("agenetico.R")

GENERACIONES = 100

iteraciones = 50

plot(NULL, xlim = c(0,GENERACIONES), ylim = c(OBJETIVO/2,OBJETIVO), ylab = "FITNESS SCORE", xlab = "GENERACIONES")
colores <- sample(grDevices::colors(), iteraciones, replace = TRUE)

for (i in 1:iteraciones){
  ag <- algogen()
  lines(c(1:length(ag[[1]])), ag[[1]], type = "l", lwd = 3, col = colores[i])
  print(paste(i,"de",iteraciones))
  flush.console()
}
