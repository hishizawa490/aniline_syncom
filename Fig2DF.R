library(tidyverse)
library(igraph)
source("functions.R")

edges<-read_csv("data07_res_edges.csv") %>% filter(add!="Cab074")

shapley<-read_csv("data10_shapley_value.csv") %>%
  mutate(add=factor(add,levels=stnames[-9]))

g<-ggplot(shapley)+
  geom_bar(aes(x=shapley,y=add,fill=add),stat="identity",width=0.8)+
  scale_fill_manual(values=colors)+
  scale_y_discrete()+
  geom_vline(xintercept=0,size=1)+
  theme_void()+
  theme(
    legend.position="none",
    strip.background = element_blank(),
    strip.text=element_blank(),
    aspect.ratio=2,
    axis.ticks=element_line(color="black",size=1.5),
    axis.ticks.length=unit(0.5,"lines"),
    axis.line=element_line(color="black",size=1.5),
    axis.text=element_text(color="black",size=30),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank()
  )
win.graph(10,10)
g
ggsave("Fig2D.pdf")


pdata<-read_csv("data09_functint.csv")
pdata$dif[pdata$p>0.05]<-NA
g <- igraph::graph_from_data_frame(pdata %>% arrange(abs(dif)), directed = F, vertices =c("Pse056","Sph075",rev(stnames[3:8])))
angle <- seq(0, 2 * pi, length.out = 9)[1:8]
layout_circle <- cbind(cos(angle), sin(angle))
V(g)$size <- 10
V(g)$color<-"white"
V(g)$frame.color<-"black"
E(g)$width <-80 * abs(E(g)$dif)-1

E(g)$color <- colorRampPalette(c("blue4" ,"transparent","red3"))(100)[as.numeric(cut(E(g)$dif, breaks = seq(min(pdata$dif,na.rm=T),-min(pdata$dif,na.rm=T),length.out=101),include.lowest=T))]
win.graph(5,5)
pdf("Fig2F.pdf") 
plot(g,
     layout = layout_circle,
     vertex.label = NA,
     vertex.frame.width=1.8,
     edge.curved = 0)
dev.off()

