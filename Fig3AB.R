library(tidyverse)
library(agricolae)
source("functions.R")

edges_res<-read_csv("data07_res_edges.csv")

g<-ggplot(data=edges_res %>% filter(nspecies!=1),aes(x=func_bg,y=eff,color=add))+
  stat_smooth(method="lm",se=F,size=1.2)+
  geom_point(size=3.5,alpha=0.7)+
  theme_bw(base_size=10)+
  scale_color_manual(values=rev(colors))+
  scale_x_continuous(breaks=c(1,1.5,2))+
  theme(
    aspect.ratio=1,
    legend.position="none",
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
ggsave("Fig3A.pdf")


edges_gfp<-edges_gfp %>% mutate(eff_res=edges_res$eff)

for(i in 1:8){
  g<-ggplot(data=edges_gfp %>% filter(nspecies!=1,add==stnames[i]),aes(x=eff,y=eff_res))+
    geom_point(size=3.5,alpha=0.7,color=colors[i])+
    stat_smooth(method="lm",se=F,size=1.2,color="black")+
    scale_x_continuous(limits=c(-1,1.3),breaks=c(-1,0,1))+
    scale_y_continuous(limits=c(-0.75,1),breaks=c(-0.5,0,0.5))+
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
  ggsave(str_c("Fig3B/",stnames[i],".pdf"))
}

