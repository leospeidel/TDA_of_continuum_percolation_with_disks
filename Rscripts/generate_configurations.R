#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

options(scipen=999)

d <- as.numeric(args[1])
N <- as.numeric(args[2])
filename <- args[3]

coordinates <- matrix(runif(N*2,min=0,max=1),N,2)
write.table(coordinates, filename, row.names=F, col.names=F)
