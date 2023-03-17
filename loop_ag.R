source("agenetico.R")


iteraciones <- 20

dd <- c()

for (i in 1:iteraciones){
  ag <- algogen()
  dd <- rbind(dd,ag[[1]])
  print(paste(i,"de",iteraciones))
  flush.console()
}

plot(NULL,
     xlim = c(0,GENERACIONES),
     ylim = c(0,max(dd)),
     ylab = "FITNESS SCORE",
     xlab = "GENERACIONES"
     )
colores <- sample(grDevices::colors(), iteraciones, replace = TRUE)

for (i in 1:iteraciones){
  lines(c(1:length(dd[i,])), dd[i,], type = "l", lwd = 3, col = colores[i])
}
