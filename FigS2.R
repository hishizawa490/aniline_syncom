library(tidyverse)
source("functions.R")

d<-read_csv("data12_aniline_degradation.csv") %>% group_by(species) %>%
  summarise(mean=mean(degradation_rate),sd=sd(degradation_rate))
  

g<-ggplot(data=d)+
  geom_bar(aes(x=species,y=mean,fill=species),stat="identity")+
  geom_errorbar(aes(x=species,ymax=mean+sd,ymin=mean-sd),width=0.2)+
  scale_fill_manual(values=c("gray40",rev(colors)[-8],"gray2",colors[1]))+
  scale_y_continuous(expand=c(0,0.2))+
  theme_bw()+
  theme(
    aspect.ratio=0.5,
    panel.grid=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(color="black",size=0.8),
    axis.text.x=element_text(color="black",size=14,angle=45,hjust=1),
    axis.text.y=element_text(color="black",size=14),
    axis.title=element_blank(),
    legend.position="none"
  )
win.graph(8,6)
g
ggsave("FigS2.pdf")
