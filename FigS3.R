library(tidyverse)
source("functions.R")

d<-read_csv("data14_resazurin_vs_aniline.csv")

g<-ggplot(data=d)+
  geom_point(aes(x=RFU,y=degradation),size=5,color="blue4")+
  stat_smooth(aes(x=RFU,y=degradation),method="lm",color="black")+
  theme_bw()+
  theme(
    aspect.ratio=1,
    axis.ticks=element_line(color="black",size=1.5),
    axis.ticks.length=unit(0.5,"lines"),
    axis.line=element_line(color="black",size=1.5),
    axis.text=element_text(color="black",size=30),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank()
  )
win.graph(12,10)
g
ggsave("FigS3A.pdf")



d<-read_csv("data15_GFP_vs_CFU.csv") %>% mutate(CFU=CFU/10)
g<-ggplot(data=d)+
  geom_point(aes(x=RFU,y=CFU),size=5,color="blue4")+
  stat_smooth(aes(x=RFU,y=CFU),method="lm",color="black")+
  theme_bw()+
  theme(
    aspect.ratio=1,
    axis.ticks=element_line(color="black",size=1.5),
    axis.ticks.length=unit(0.5,"lines"),
    axis.line=element_line(color="black",size=1.5),
    axis.text=element_text(color="black",size=30),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank()
  )
win.graph(12,10)
g
ggsave("FigS3B.pdf")