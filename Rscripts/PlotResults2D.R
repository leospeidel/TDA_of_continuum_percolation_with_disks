library(data.table)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
source("../Rscripts/plot_gg.R")
options(scipen=999)

LoadData <-function(N){
  
  radius <- N * pi;

  print(N)
  
  ##
  #read Persistence diagram from output of dionysus
  TDA_data <- as.data.frame(fread(paste("./birth_persistence/2d_N",N,".TDA",sep=""), skip = 6, fill = T))
 
  index_for_separating <- which(is.na(TDA_data[,2]))
  d0 <- as.matrix((TDA_data[1:(index_for_separating[1]-1),]) * radius)
  d1 <- as.matrix((TDA_data[(index_for_separating[2]+1):length(TDA_data[,2]),]) * radius)

  return(list(d0=d0, d1=d1))
  
}

###########################

PlotBirthPersistenceDiagram <- function(N){

  betti <- LoadData(N)

  d1 <- ggplot(data.frame(birth=betti$d1[,1], persistence=betti$d1[,2]-betti$d1[,1]),aes(x=birth,y=persistence)) + stat_binhex(colour="white",na.rm=TRUE)+
    scale_fill_gradientn(colours=c("yellow","black"),name = "Counts\n",na.value=NA, trans = "log10", breaks = c(1,10,100,1000,10000,100000), labels = expression(10^0, 10^1, 10^2, 10^3, 10^4, 10^5)) +
    geom_vline(xintercept = 1.128) +
    xlab(expression(eta)) + ylab("persistence") + ggtitle("(a)") +
    scale_x_continuous_Publication(limits=c(0,10)) + 
    scale_y_continuous_Publication(limits=c(0,20)) +
    theme_Publication(base_size = 35) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key.width=unit(1, "cm"),
          legend.key.height=unit(3,"cm"), plot.title = element_text(size = rel(1.3))) 


  return(d1)
  
}

PlotTopPersistingInvariants <- function(){
  
  library(extrafont)
  library(RColorBrewer)
  options(scipen = 999)

  cols <- brewer.pal(4, "Dark2")
  
  N <- 10000
  top_persisting_invariant_N10000 <- read.table(paste("./top_persisting/TDA_",N,"/2d_N",N,".TDA",sep=""))
  N <- 100000
  top_persisting_invariant_N100000 <- read.table(paste("./top_persisting/TDA_",N,"/2d_N",N,".TDA",sep=""))
  N <- 1000000
  top_persisting_invariant_N1000000 <- read.table(paste("./top_persisting/TDA_",N,"/2d_N",N,".TDA",sep=""))
  N <- 10000000
  top_persisting_invariant_N10000000 <- read.table(paste("./top_persisting/TDA_",N,"/2d_N",N,".TDA",sep=""))

  se <- function(x) sqrt(var(x)/length(x))

  top_persisting_invariant_N10000$N <- "10,000"
  top_persisting_invariant_N100000$N <- "100,000"
  top_persisting_invariant_N1000000$N <- "1,000,000"
  top_persisting_invariant_N10000000$N <- "10,000,000"
  df <- rbind(top_persisting_invariant_N10000, top_persisting_invariant_N100000, top_persisting_invariant_N1000000, top_persisting_invariant_N10000000)
  mean   <- aggregate(V1 ~ N, data=df, mean)
  variance <- aggregate(V1 ~ N, data=df, var)
  limits <- aggregate(V1 ~ N, data=df, se)
  mean$N <- factor(mean$N,levels=c("10,000","100,000","1,000,000","10,000,000"))
  mean$N <- c(1000000,10000,10000000,100000)
  variance$N <- factor(variance$N,levels=c("10,000","100,000","1,000,000","10,000,000"))
  variance$N <- c(1000000,10000,10000000,100000)
  
  p1 <- ggplot(mean, aes(x=N, y=V1)) + geom_point(size = 3) + 
    geom_hline(yintercept = 1.128, color = "red", lwd = 1.3) +
    geom_errorbar(aes(ymax = V1 + 1.96*limits[,2], ymin = V1 - 1.96*limits[,2]), width=0.25) +
    labs(x = expression(italic(N)), y = expression(E(eta))) +
    scale_x_continuous_sci_Publication(trans = "log10", limits = c(1e3,1e8), breaks = c(1e4,1e5,1e6,1e7)) +
    scale_y_continuous_Publication(limits = c(1,1.4)) + theme_Publication(base_size = 35) + ggtitle("(b)") + theme(plot.title = element_text(size = rel(1.3)))
  
  p2 <- ggplot(variance, aes(x=N, y=V1)) + geom_point(size = 3) +
    geom_errorbar(aes(ymax = V1 + limits[,2], ymin = V1 - limits[,2]), width=0.25) +
    labs(x = expression(N), y = expression(Var(eta))) + theme_Publication() + 
    scale_x_continuous_sci_Publication(trans = "log10", limits = c(1e3,1e8), breaks = c(1e4,1e5,1e6,1e7)) +
    scale_y_continuous_Publication(limits = c(0,0.1))

  return(p1)
  
}


PlotPaperFig3 <- function(){

  d1 <- PlotBirthPersistenceDiagram(10000)
  p1 <- PlotTopPersistingInvariants()

  pdf("plot_birth_persistence.pdf", width = 20, height = 10)
  grid.draw(cbind(ggplotGrob(d1),ggplotGrob(p1), size = "last"))
  dev.off()
  

}

PlotPaperFig4 <- function(){ 
 
  options(scipen = 999)
  
  cols <- brewer.pal(4, "Dark2")
  
  N <- 100000
  top_persisting_invariant_N100000 <- read.table(paste("./estimate_constants/TDA_",N,"/2d_N",N,".TDA",sep=""))
  N <- 1000000
  top_persisting_invariant_N1000000 <- read.table(paste("./estimate_constants/TDA_",N,"/2d_N",N,".TDA",sep=""))
  N <- 10000000
  top_persisting_invariant_N10000000 <- read.table(paste("./estimate_constants/TDA_",N,"/2d_N",N,".TDA",sep=""))
  
  top_persisting_invariant_N100000$N <- "100,000"
  top_persisting_invariant_N100000$x <- seq(0.01,20,0.01)
  top_persisting_invariant_N100000$scaled_betti <- top_persisting_invariant_N100000$V1/100000
  top_persisting_invariant_N1000000$N <- "1,000,000"
  top_persisting_invariant_N1000000$x <- seq(0.01,20,0.01)
  top_persisting_invariant_N1000000$scaled_betti <- top_persisting_invariant_N1000000$V1/1000000
  top_persisting_invariant_N10000000$N <- "10,000,000"
  top_persisting_invariant_N10000000$x <- seq(0.01,20,0.01)
  top_persisting_invariant_N10000000$scaled_betti <- top_persisting_invariant_N10000000$V1/10000000
  df <- rbind(top_persisting_invariant_N100000, top_persisting_invariant_N1000000, top_persisting_invariant_N10000000)
  
  df$N <- factor(df$N,levels=c("100,000","1,000,000","10,000,000"))
  
  data <- subset(top_persisting_invariant_N10000000, top_persisting_invariant_N10000000$x < 1)
  reg <- lm(log(data$scaled_betti) + 1 ~ log(data$x))
  y <- reg$coefficients[1] + reg$coefficients[2] * log(seq(0.02,0.5,0.01))  
  regression <- data.frame(x = seq(0.02,0.5,0.01), y = exp(y))
  print(reg$coefficients[2])
  
  p2 <- ggplot(df) + geom_line(aes(x=x, y = scaled_betti, colour = N, linetype = N), size = 1.2) +
    geom_vline(xintercept = 1.128, color = "black", lwd = 1.2) +
    geom_line(data=regression, aes(x = x, y = y)) +
    xlab(expression(eta)) + ylab(expression(beta[1]/italic(N))) + 
    scale_x_continuous_sci_Publication(limits = c(1e-2,1e2), trans = "log10", breaks = c(1e-2,1e0,1e2)) + 
    scale_y_continuous_sci_Publication(limits = c(1e-7,10), trans = "log10") +
    theme_Publication(legend.position="none") + 
    theme(text = element_text(size = 30),legend.title=element_blank()) 
  
  data <- subset(top_persisting_invariant_N10000000, top_persisting_invariant_N10000000$x > 8)
  data <- subset(data, data$x < 18)
  reg <- lm(log(data$scaled_betti) + 1 ~ data$x)
  y <- reg$coefficients[1] + reg$coefficients[2] * seq(8,15,0.1)  
  regression <- data.frame(x = seq(8,15,0.1), y = exp(y))
  print(reg$coefficients[2])
  
  p3 <- ggplot(df) + geom_line(aes(x=x, y = scaled_betti, colour = N, linetype = N), size = 1.2) +
    geom_vline(xintercept = 1.128, color = "black", lwd = 1.2) +
    geom_line(data=regression, aes(x = x, y = y)) +
    xlab(expression(eta)) + ylab("") + 
    scale_x_continuous_Publication(limits = c(0,20),breaks = c(0,5,10,15,20)) + 
    scale_y_continuous_sci_Publication(limits = c(1e-7,10), trans = "log10") +
    theme_Publication(legend.position=c(.7, .8)) + 
    theme(text = element_text(size = 30),legend.title=element_blank())
  
  plot_filename <- "plot_estimating_constant.pdf"
  g <- plot_grid(p2,p3, ncol = 2, labels = c("(a)", "(b)"), label_size = 35)
  ggsave(g, file = plot_filename, device = "pdf", height = 10, width = 20)
  embed_fonts(plot_filename, outfile=plot_filename)
  
}


PlotPaperFig3()
PlotPaperFig4()

