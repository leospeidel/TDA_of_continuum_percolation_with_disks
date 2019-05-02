options(scipen=999)

args = commandArgs(trailingOnly=TRUE)
N <- as.numeric(args[1])
filename <- args[2]

LoadData <-function(N){
  
  radius <- pi*N;
 
  ##
  #read Persistence diagram from output of dionysus
  TDA_data <- read.table(filename, skip = 6, fill = T)  
 
  index_for_separating <- which(is.na(TDA_data[,2]))
  d0 <- as.matrix((TDA_data[1:(index_for_separating-1),]) * radius)
  d1 <- as.matrix((TDA_data[(index_for_separating+1):length(TDA_data[,2]),]) * radius)

  return(list(d0=d0, d1=d1))
  
}

betti <- LoadData(N)
index <- which.max(betti$d1[,2] - betti$d1[,1])
write.table(betti$d1[index,1],filename, row.names =F, col.names=F, append=F)
