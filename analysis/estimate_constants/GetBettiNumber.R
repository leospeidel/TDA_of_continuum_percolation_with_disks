options(scipen=999)

args = commandArgs(trailingOnly=TRUE)
N <- as.numeric(args[1])
filename <- args[2]

LoadData <-function(N){
  
  radius <- N*pi;
 
  ##
  #read Persistence diagram from output of dionysus
  TDA_data <- read.table(filename, skip = 6, fill = T)  
 
  index_for_separating <- which(is.na(TDA_data[,2]))
  d0 <- as.matrix((TDA_data[1:(index_for_separating-1),]) * radius)
  d1 <- as.matrix((TDA_data[(index_for_separating+1):length(TDA_data[,2]),]) * radius)

  return(list(d0=d0, d1=d1))
  
}


betti <- LoadData(N)
index <- which(betti$d1[,2] < 20)

h1 <- hist(betti$d1[index,1], breaks = seq(0,20,0.01))
h2 <- hist(betti$d1[index,2], breaks = seq(0,20,0.01))

res <- cumsum(h1$counts) - cumsum(h2$counts)

write.table(res,filename, row.names =F, col.names=F)
