library(R.utils) # mamba install -c conda-forge r-r.utils

decs <- seq(1,3000)
bins <- intToBin(decs)
nbins <- as.numeric(bins)
bitsInDecs <- floor(log2(decs))+1

binmatrix <- c()
for (b in bins){
  bm <- as.numeric(unlist(strsplit(b,split = "")))
  binmatrix <- rbind(binmatrix,bm)
}
rownames(binmatrix) <- c(1:nrow(binmatrix))

# Como primer inento voy a intentar penalizar la distancia lineal despues la distancia
# en mutaciones y finalmente la distancia en bits cada una penalizando mas que la anterior

fit3dist <- function(d1,d2,d3){
  fs <- (1/(1+d1)/(1+d2))/(1+d3)
  return(fs)
}

fit2dist <- function(d1,d2){
  fs <- (1/(1+d1))/(1+d2)
  return(fs)
}

fit1dist <- function(d1){
  fs <- (1/1+d1)
  return(fs)
}


asBin <- function(x){
  as.numeric(intToBin(x))
}

asGeneSumBin <- function(x){
  sum(as.numeric(unlist(strsplit(intToBin(target),split = ""))))
}

getBitCount <- function(x){
  floor(log2(target))+1
}

getLinearDistance <- function(target,x){
  abs(target-x)
}

getBitDistance <- function(targetBitCount,XbitCount){
  abs(targetBitCount-XbitCount)
}

getBinaryAttributes <- function(target){
  binvec <- as.numeric(unlist(strsplit(intToBin(target),split = "")))
  if (length(binvec) > BITS){
    print("ERROR: variable BITS de tamaño insuficiente para expresar el OBJETIVO")
    binvec <- NULL
  } else {
    while(length(binvec) < BITS){
      binvec <- c(0,binvec)
   }
    bits <- floor(log2(target))+1
  }
  return(list(binvec,bits))
}

getMutationalDistance <- function(matrix){
  BinVector <- getBinaryAttributes(OBJETIVO)[[1]]
  apply(matrix,1,function(x)sum(BinVector!=x))
}


## dl <- getLinearDistance(target,decs)
## dm <- getMutationalDistance(binmatrix)
## db <- getBitDistance(targetbit,bitsInDecs)


findbinary <- function(target){


}

