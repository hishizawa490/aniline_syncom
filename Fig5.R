library(tidyverse)
library(igraph)
library(randomForest)
library(MLmetrics)
library(vivid)
source("functions.R")

d<-read_csv("data06_summary.csv")

d<-d  %>% rowwise() %>% filter(nspecies>=2) %>%
  mutate(Sph075=substr(set,2,2),Pse056=substr(set,3,3),Bac050=substr(set,4,4),Bos046=substr(set,5,5),
         Mic039=substr(set,6,6),Sph034=substr(set,7,7),Pse033=substr(set,8,8),Ach031=substr(set,9,9)) %>% ungroup() 

ave_d<-d %>% group_by(set) %>% summarise(res_AUC=mean(res_AUC),gfp_AUC=mean(gfp_AUC)) %>% 
  left_join(d %>% filter(rep==1) %>% dplyr::select("set","nspecies",14:21),by="set")

rfmodel<-rfrep(ave_d,test_n=5:9,train_n=3:4,sample=1)
viviRf<-vivi(fit=rfmodel[[2]],data=ave_d %>% filter(nspecies>=5) %>% dplyr::select(5:12,res_AUC),response="res_AUC")
viviRf<-viviRf[ord<-order(diag(viviRf),decreasing=T),ord]
Vimp<-diag(viviRf)
nodes<-names(Vimp)
int_list <- which(upper.tri(viviRf), arr.ind = TRUE)
int <- data.frame(
  from = nodes[int_list[,1]],
  to = nodes[int_list[,2]],
  weight = viviRf[int_list]
  ) %>% arrange(weight)

Vimp<-Vimp[stnames[c(2:1,8:3)]]
vertices<-tibble(add=stnames[c(2:1,8:3)], Vimp = Vimp)
g <- igraph::graph_from_data_frame(int, directed = FALSE, vertices=vertices)
angle <- seq(0, 2 * pi, length.out = 9)[1:8]
layout_circle <- cbind(cos(angle), sin(angle))
V(g)$size <- 7 + 300 * Vimp
Vimp_clipped <- pmax(0, pmin(Vimp, 0.05))
color_indices <- as.numeric(cut(Vimp_clipped, breaks = seq(0, 0.05, length.out = 101), include.lowest = TRUE))
V(g)$color <- colorRampPalette(c("#D1E1D1", "#2F5E30"))(100)[color_indices]
E(g)$width <- 700 * E(g)$weight 
E(g)$color <- colorRampPalette(c("transparent", "#33219F"))(100)[
  as.numeric(cut(E(g)$weight, breaks = seq(0, 0.0134, length.out = 101), include.lowest = TRUE))
]
win.graph(5,5)
pdf(str_c("Fig5X.pdf")) 
plot(g,
     layout = layout_circle,
     vertex.label = NA,
     vertex.frame.color = NA,
     rescale = FALSE,            
     xlim = c(-1.1, 1.1),      
     ylim = c(-1.1, 1.1), 
     edge.curved = 0)
dev.off()

pdata<-left_join(read_csv("data09_functint.csv"),int,by=c("from","to")) %>% na.omit()
cor.test(abs(pdata$dif),pdata$weight)

g<-ggplot()+
  geom_point(data=pdata,aes(x=weight,y=abs(dif)),size=6,color="#33219F")+
  scale_x_continuous(limits=c(0,0.0134))+
  scale_y_continuous(limits=c(0,0.16))+
  geom_smooth(data=pdata,aes(x=weight,y=abs(dif)),method="lm",col="black",linetype=1)+
  theme_bw()+
  theme(
    legend.position="none",
    strip.background = element_blank(),
    strip.text=element_blank(),
    aspect.ratio=1,
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
ggsave(str_c("Fig5X.pdf")) 

