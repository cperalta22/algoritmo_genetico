# Intento 1 de un algoritmo genetico
# CPA 2023

BITS <- 10
OBJETIVO <- 10
TAMANO_POBLACION <- 20
GENERACIONES <- 20
MUTACION_FREC <- 0.01
PROB_CRUZA <- 0.8

fitness_eval_onemax <- function(individuo){
  fitscore <- sum(individuo)/OBJETIVO
  return(fitscore)
}

fitness_eval_population <- function(population,fitness_eval){
  fit_pop <- apply(population,1,fitness_eval)
  fit_propo <- fit_pop/sum(fit_pop)
  fit_propo_sum <- cumsum(fit_propo)
  return(list(fit_pop, fit_propo, fit_propo_sum))
}

individual_create <- function(bits){
  indi <- sample(c(0,1), size = bits, replace = T)
  return(indi)
}

pop_create <- function(pob_size,bits) {
  my_pop <- c()
  for (i in 1:pob_size){
    ind <- individual_create(bits)
    my_pop <- rbind(my_pop,ind)
  }
  rownames(my_pop) <- c(1:nrow(my_pop))
  return(my_pop)
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

make_babies <- function(PROB_CRUZA, poblacion){}


proportional_selection <- function(poblacion, fitness_eval){
  eval_metrics <- fitness_eval_population(poblacion, fitness_eval)
  cumulative_proportions <- eval_metrics[[3]]
  for (i in 1:nrow(poblacion)){

  }
}
