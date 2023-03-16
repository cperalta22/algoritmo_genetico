# Intento 1 de un algoritmo genetico
# CPA 2023

BITS              <- 100
OBJETIVO          <- 100
TAMANO_POBLACION  <- 1000
GENERACIONES      <- 500
MUTACION_FREC     <- 0.01
PROB_CRUZA        <- 0.8
ELITISMO          <- TRUE
FITNESS_EVAL      <- "onemax"

onemax <- function(individuo){
  fitscore <- sum(individuo)
  return(fitscore)
}


fitness_eval_population <- function(population){
  fit_pop <- apply(population,1,FITNESS_EVAL)
  fit_propo <- fit_pop/sum(fit_pop)
  fit_propo_sum <- cumsum(fit_propo)
  best_fit <- sort(fit_pop,decreasing = T)[1]
  best_idx <- as.numeric(labels(best_fit))
  best <- population[best_idx,]
  best_fit <- as.numeric(best_fit)
  return(list(fit_pop,        # 1 Scores de Fitness por cada individuo
              fit_propo,      # 2 Fracción ponderada del fitness por cada individuo
              fit_propo_sum,  # 3 Suma ponderada de los fitness score
              best,           # 4 Mejor individuo de esta población
              best_fit        # 5 Mejor score de esta población
              )
         )
}

individual_create <- function(){
  indi <- sample(c(0,1), size = BITS, replace = T)
  return(indi)
}

pop_create <- function() {
  my_pop <- c()
  for (i in 1:TAMANO_POBLACION){
    ind <- individual_create()
    my_pop <- rbind(my_pop,ind)
  }
  rownames(my_pop) <- c(1:nrow(my_pop))
  return(my_pop)
}

proportional_selection <- function(poblacion){
  whowon <-c()
  eval_metrics <- fitness_eval_population(poblacion)
  cumulative_proportions <- as.vector(eval_metrics[[3]])
  lotto_tickets <- sample(seq(min(cumulative_proportions),1,0.0001),nrow(poblacion))
  for (ticket in lotto_tickets){
    winner <- sort(cumulative_proportions[cumulative_proportions >= ticket ])[1]
    whowon <- c(whowon,which(cumulative_proportions == winner)[1])
  }
  new_pop <- poblacion[whowon,]
  rownames(new_pop) <- c(1:nrow(new_pop))
  if (ELITISMO){
    new_pop[1,]<- eval_metrics[[4]]
  }
  return(list(new_pop,                # 1 población seleccionada para reproducirse y/o mutar
              eval_metrics[[5]],      # 2 mejor fitness score de esta generacion
              eval_metrics[[4]],      # 3 mejor individuo de la generacion
              mean(eval_metrics[[1]]) # 4 media del fitness score de esta generación
              ))
}

make_gene_mutation <- function(gene){
  mutate <- sample(c(TRUE,FALSE),size = 1, prob = c(MUTACION_FREC,1-MUTACION_FREC))
  if(mutate){
    if ( gene == 0 ){
      gene <- 1
    }
    else {
      gene <- 0
    }
  }
  return(gene)
}

mutate_population <- function(population){
  mp <- apply(population,c(1,2),make_gene_mutation)
  return(mp)
}

make_babies <- function(poblacion){
  new_generation <- c()
  for (i in seq(1,TAMANO_POBLACION-1, by = 2)){
    j <- i+1
    cruza <- sample(c(TRUE,FALSE),size = 1,prob = c(PROB_CRUZA,1-PROB_CRUZA))
    if (cruza){
      punto_corte <- sample(c(2:BITS-1),size = 1)
      bebi1<- as.vector(c(poblacion[i,1:punto_corte],poblacion[j,(punto_corte+1):BITS]))
      bebi2 <- as.vector(c(poblacion[j,1:punto_corte],poblacion[i,(punto_corte+1):BITS]))
      new_generation <- rbind(new_generation, bebi1, bebi2)
    } else {
      new_generation <- rbind(new_generation, poblacion[i,], poblacion[j,])
    }
  }
  new_generation <- mutate_population(new_generation)
  if(ELITISMO){
     sacrificado<- sample(1:TAMANO_POBLACION, size = 1)
     new_generation[sacrificado,] <- poblacion[1,]
  }
  rownames(new_generation) <-c(1:TAMANO_POBLACION)
  return(new_generation)
}

algogen <- function(bits = BITS,
                    objetivo = OBJETIVO,
                    pop_size = TAMANO_POBLACION,
                    generaciones = GENERACIONES,
                    prop_cruza = PROB_CRUZA,
                    frec_mutacion = MUTACION_FREC,
                    elitismo = ELITISMO,
                    fitness_function = FITNESS_EVAL
                    ){
  meanFS <- c()     # para guardar el fitness score promedio de cada generacion
  bestFS <- c()  # para guardar el mejor fitness score de cada generacion
  bestInd <-c() # mejor individuo

  pob <- pop_create()

  for (g in 1:GENERACIONES){
    ps <- proportional_selection(pob)
    pob <- ps[[1]]
    bestFS <- c(bestFS, ps[[2]])
    meanFS <- c(meanFS, ps[[4]])
    bestInd <- ps[[3]]
    pob <- make_babies(pob)
    if(ps[[2]]==OBJETIVO){
      break
    }
  }
  return(list(
              bestFS, # 1 vector de mejores fitness score
              meanFS, # 2 vector de fitness score promedio
              bestInd # 3 el mejor individuo entre todos
              )
         )
}

