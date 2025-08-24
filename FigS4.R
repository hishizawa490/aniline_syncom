library(tidyverse)
library(agricolae)
source("functions.R")

edges<-read_csv("data08_gfp_edges.csv")


for(i in 1:8){
  g<-ggplot()+
    geom_segment(data=edges %>% rowwise() %>% filter(add!="Cab074") %>% filter(add!=stnames[i]),
                 aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func),
                 linewidth=1,color="gray",alpha=0.5)+
    geom_segment(data=edges %>% rowwise() %>% filter(add==stnames[i]),
                 aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func),
                 linewidth=1.4,color=colors[i],alpha=1)+
    geom_point(data=edges %>% filter(add!="MA074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.7)+
    geom_point(data=edges %>% filter(add=="MA074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.7)+
    scale_x_continuous(breaks=1:9)+
    scale_y_continuous(limits=c(0.83,1.95),breaks=c(1,1.5,2))+
    theme_bw(base_size=10)+
    theme(
      legend.position="none", 
      aspect.ratio=1,
      axis.ticks=element_line(color="black",size=2.4),
      axis.ticks.length=unit(1,"lines"),
      axis.line=element_line(color="black",size=2.4),
      axis.text=element_text(color="black",size=50),
      axis.title=element_blank(),
      panel.grid=element_blank(),
      panel.border=element_blank(),
    )
  win.graph(10,10)
  g
  ggsave(str_c("FigS4/",stnames[i],".pdf"))
}