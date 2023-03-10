# Intento 1 de un algoritmo genetico
# CPA 2023

BITS <- 40
OBJETIVO <- 12
TAMANO_POBLACION <- 10000
SOBREVIVIENTES <- 500
GENERACIONES <- 20
MUTACION_FREC <- 0.01

individual_create <- function(bits){
  indi <- sample(c(0,1), size = bits, replace = T)
  return(indi)
}

pop_create <- function(pob_size,bits) {
  my_pop <- list()
  for (i in 1:pob_size){
    indi <- individual_create(bits)
    my_pop[[i]] <- indi
  }
  return(my_pop)
}

fitness_eval <- function(individuo, objetivo){
  dist_al_objetivo <- abs(objetivo - sum(individuo))
  return(dist_al_objetivo)
}

make_mutation <- function(mut_frecuency, individuo){
  mutate <- sample(c(TRUE,FALSE),size = 1, prob = c(mut_frecuency,1-mut_frecuency))
  if(mutate){
    where <- sample(c(1:length(individuo)),size = 1)
    if (individuo[where] == 0 ){
      individuo[where] <- 1
    }
    else {
      individuo[where] <- 0
    }
  }
}

make_babies <- function(){}


natural_selection <- function(poblacion, sobrevivientes){

}
