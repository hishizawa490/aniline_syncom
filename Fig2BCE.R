library(tidyverse)
library(agricolae)
source("functions.R")

edges<-read_csv("data07_res_edges.csv")

g<-ggplot(data=edges %>% rowwise() %>% filter(add!="Cab074"))+
  geom_segment(aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func,color=add),
               linewidth=0.8,alpha=0.7)+
  geom_point(data=edges %>% filter(add!="Cab074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.3)+
  geom_point(data=edges %>% filter(add=="Cab074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.3)+
  scale_x_continuous(breaks=1:9,name="N of species")+
  scale_color_manual(values=rev(colors))+
  scale_y_continuous(limits=c(0.83,1.95),breaks=c(1,1.5,2))+
  theme_bw(base_size=10)+
  theme(
    legend.position="none", 
    aspect.ratio=1,
    axis.ticks=element_line(color="black",size=1.2),
    axis.ticks.length=unit(0.5,"lines"),
    axis.line=element_line(color="black",size=1.2),
    axis.text=element_text(color="black",size=27),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank()
  )
win.graph(10,10)
g
ggsave("Fig2B.pdf")

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
  ggsave(str_c("Fig2C/",stnames[i],".pdf"))
}

g<-ggplot()+
  geom_segment(data=edges %>% rowwise() %>% filter(add!="Cab074") %>% filter(add!="Sph034"),
               aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func),
               linewidth=1,color="gray",alpha=0.5)+
  geom_segment(data=edges %>% rowwise() %>% filter(add=="Sph034","Pse033" %in% set_to_namelist(bg)),
               aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func),
               linewidth=1.7,color=colors[6],alpha=1)+
  geom_point(data=edges %>% filter(add!="MA074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.7)+
  geom_point(data=edges %>% filter(add=="MA074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.7)+
  scale_x_continuous(breaks=1:9)+
  scale_y_continuous(limits=c(0.83,1.95),breaks=c(1,1.5,2))+
  theme_bw(base_size=10)+
  theme(
    legend.position="none", 
    aspect.ratio=1,
    axis.ticks=element_line(color="black",size=2),
    axis.ticks.length=unit(0.6,"lines"),
    axis.line=element_line(color="black",size=2),
    axis.text=element_text(color="black",size=35),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank(),
  )
win.graph(10,10)
g
ggsave("Fig2E1.pdf")

g<-ggplot()+
  geom_segment(data=edges %>% rowwise() %>% filter(add!="Cab074") %>% filter(add!="Sph034"),
               aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func),
               linewidth=1,color="gray",alpha=0.5)+
  geom_segment(data=edges %>% rowwise() %>% filter(add=="Sph034","Pse033" %!in% set_to_namelist(bg)),
               aes(x=nspecies-1,xend=nspecies,y=func_bg,yend=func),
               linewidth=1.7,color=colors[6],alpha=1)+
  geom_point(data=edges %>% filter(add!="MA074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.7)+
  geom_point(data=edges %>% filter(add=="MA074"),aes(x=nspecies,y=func),shape=21,fill="white",stroke=0.6,size=3.7)+
  scale_x_continuous(breaks=1:9)+
  scale_y_continuous(limits=c(0.83,1.95),breaks=c(1,1.5,2))+
  theme_bw(base_size=10)+
  theme(
    legend.position="none", 
    aspect.ratio=1,
    axis.ticks=element_line(color="black",size=2),
    axis.ticks.length=unit(0.6,"lines"),
    axis.line=element_line(color="black",size=2),
    axis.text=element_text(color="black",size=35),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank(),
  )
win.graph(10,10)
g
ggsave("Fig2E2.pdf")
