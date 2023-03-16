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

target <- 1500
targetbin <- as.numeric(intToBin(target))
targetgenesum <- sum(as.numeric(unlist(strsplit(intToBin(target),split = ""))))
targetBinVector <- as.numeric(unlist(strsplit(intToBin(target),split = "")))
targetbit <- floor(log2(target))+1

otbv <- targetBinVector
while(length(targetBinVector) < length(binmatrix[1,])){
  targetBinVector <- c(0,targetBinVector)
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

getMutationalDistance <- function(targetGeneSum, xGeneSum){
  abs(targetGeneSum-xGeneSum)
}

getBitDistance <- function(targetBitCount,XbitCount){
  abs(targetBitCount-XbitCount)
}

getMutDist <- function(matrix){
  apply(matrix,1,function(x)sum(targetBinVector!=x))
}


dl <- getLinearDistance(target,decs)
geneSums <- as.vector(rowSums(binmatrix))
fdm <- getMutationalDistance(targetgenesum,geneSums)
db <- getBitDistance(targetbit,bitsInDecs)
dm <- getMutDist(binmatrix)
