
filenames <- list.files(path = ".", pattern = "2d")

betti <- as.matrix(read.table(filenames[1]))
for( i in 2:length(filenames) ){
  print(filenames[i])
  betti <- betti + as.matrix(read.table(filenames[i]))
}

write.table(betti/length(filenames), "2d_N10000.TDA", row.names=F, col.names=F)

