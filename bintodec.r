library(R.utils) # mamba install -c conda-forge r-r.utils

# LIBRERIA PARA ENCONTRAR ENTEROS EN POBLACIONES BINARIAS

## decs <- seq(1,3000)
## bins <- intToBin(decs)
## nbins <- as.numeric(bins)
## bitsInDecs <- floor(log2(decs))+1

## binmatrix <- c()
## for (b in bins){
##   bm <- as.numeric(unlist(strsplit(b,split = "")))
##   binmatrix <- rbind(binmatrix,bm)
## }
## rownames(binmatrix) <- c(1:nrow(binmatrix))

fit3dist <- function(d1,d2,d3){
  fs <- (1/(1+d1)/(1+d2))/(1+d3)
  return(fs)
}

fit2dist <- function(d1,d2){
  fs <- (1/(1+d1))/(1+d2)
  return(fs)
}

fit1dist <- function(d1){
  fs <- (1/(1+d1))
  return(fs)
}

asBin <- function(x){
  as.numeric(intToBin(x))
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

getBinVector <- function(target,bits){
  binvec <- as.numeric(unlist(strsplit(intToBin(target),split = "")))
  if (length(binvec) > bits){
    print("ERROR: variable BITS de tamaño insuficiente para expresar el OBJETIVO")
    binvec <- NULL
  } else {
    while(length(binvec) < bits){
      binvec <- c(0,binvec)
   }
  }
  return(binvec)
}

getMutationalDistance <- function(x,BinVector){
  sum(BinVector!=x)
}

BinToDec <- function(x)
  #https://stackoverflow.com/questions/12892348/convert-binary-string-to-binary-or-decimal-value
    sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
