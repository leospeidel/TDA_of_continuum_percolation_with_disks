library(ggplot2)
library(gridExtra)
library(extrafont)

theme_Publication <- function(base_size=14,...) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(size = rel(1.0), hjust = -0.1, vjust = 10),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = "black"),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust = 1),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_blank(),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.key.size= unit(4.5, "line"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(size = rel(1.0)),
            plot.margin=unit(c(10,20,10,10),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold"),...
    ))
  
}


scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


scientific_10 <- function(x){
  library(scales)
  
  exp <- scientific_format()(x)
  exp <- gsub("\\+","",exp)
  labs <- parse(text=gsub("e", " %*% 10^", exp))
  #print(labs)
  labs <- parse(text=gsub("1 \\%\\*\\% ", "", labs))
  #print(labs)
  if(any(as.numeric(x) == 0)){
    labs[which(as.numeric(x) == 0)] <- "0"
  }
  
  return(labs)
}

scale_x_continuous_Publication <- function(...){
  library(scales)
  scale_x_continuous(expand = c(0, 0), ...)  
}
scale_x_continuous_sci_Publication <- function(...){
  library(scales)
  scale_x_continuous(expand = c(0, 0), label=scientific_10, ...)  
}

scale_y_continuous_Publication <- function(...){
  library(scales)
  scale_y_continuous(expand = c(0, 0), ...)  
}
scale_y_continuous_sci_Publication <- function(...){
  library(scales)
  scale_y_continuous(expand = c(0, 0), label=scientific_10, ...)  
}

scale_x_discrete_Publication <- function(...){
  library(scales)
  scale_x_discrete(expand = c(0, 0), ...)  
}
scale_x_discrete_sci_Publication <- function(...){
  library(scales)
  scale_x_discrete(expand = c(0, 0), label=scientific_10, ...)  
}

scale_y_discrete_Publication <- function(...){
  library(scales)
  scale_y_discrete(expand = c(0, 0), ...)  
}
scale_y_discrete_sci_Publication <- function(...){
  library(scales)
  scale_y_discrete(expand = c(0, 0), label=scientific_10, ...)  
}

axis_lines <- function(...){
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)) 
}

plot_gg <- function(p){
  return(p+scale_colour_Publication()+theme_Publication())
}
