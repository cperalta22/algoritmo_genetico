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

fit1 <- function(dlin,dmut,dbit){
  fs <- ((1/(1+dlin))/(1+dmut))/(1+dbit)
  return(fs)
}

fit2 <- function(dlin,dbit){
  fs <- (1/(1+dlin))/(1+dbit)
  return(fs)
}

target <- 260
targetbin <- as.numeric(intToBin(target))
targetgenesum <- sum(as.numeric(unlist(strsplit(intToBin(target),split = ""))))
targetbit <- floor(log2(target))+1

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

getMutationalDistance <- function(targetGeneSum, xGeneSum){
  abs(targetGeneSum-xGeneSum)
}

getBitDistance <- function(targetBitCount,XbitCount){
  abs(targetBitCount-XbitCount)
}


dl <- getLinearDistance(target,decs)
geneSums <- as.vector(rowSums(binmatrix))
dm <- getMutationalDistance(targetgenesum,geneSums)
db <- getBitDistance(targetbit,bitsInDecs)
