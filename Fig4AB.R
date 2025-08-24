library(vivid)
library(randomForest)
library(tidyverse)
library(igraph)
library(MLmetrics)

source("functions.R")

d<-read_csv("data06_summary.csv")

d<-d  %>% rowwise() %>% filter(nspecies>=2) %>%
  mutate(Sph075=substr(set,2,2),Pse056=substr(set,3,3),Bac050=substr(set,4,4),Bos046=substr(set,5,5),
         Mic039=substr(set,6,6),Sph034=substr(set,7,7),Pse033=substr(set,8,8),Ach031=substr(set,9,9)) %>% ungroup() 

ave_d<-d %>% group_by(set) %>% summarise(res_AUC=mean(res_AUC),gfp_AUC=mean(gfp_AUC)) %>% 
  left_join(d %>% filter(rep==1) %>% dplyr::select("set","nspecies",14:21),by="set")

rfmodel<-rfrep(ave_d,train_n=3)
pred<-predict(rfmodel[[2]],filter(ave_d,nspecies %in% 5:9) %>% dplyr::select(5:12,res_AUC))

g<-ggplot(data=ave_d %>% filter(nspecies>=5) %>% mutate(pred=as.numeric(pred)))+
  geom_point(aes(x=pred,y=res_AUC,color=as.factor(nspecies)),size=5,alpha=0.8)+
  scale_color_manual(values=rev(viridisLite::viridis(7))[2:6])+
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
  )+
  coord_equal(xlim=c(min(c(pred,filter(d,nspecies>=5)$res_AUC)),max(c(pred,filter(d,nspecies>=5)$res_AUC))),
              ylim=c(min(c(pred,filter(d,nspecies>=5)$res_AUC)),max(c(pred,filter(d,nspecies>=5)$res_AUC))))
win.graph(10,10)
g
ggsave("Fig4A.pdf")

stats<-tribble(~"train_n",~"sample",~"meanr",~"sd")
for(i in 2:7){
  for(j in c(0.25,0.5,1)){
    rfmodel<-rfrep(ave_d,train_n=i,sample=j)
    stats<-stats %>% add_row(
      "train_n"=i,
      "sample"=j,
      "meanr"=mean(rfmodel[[1]]$r),
      "sd"=sd(rfmodel[[1]]$r))
  }
}
write.csv(stats,"data11_rf_model_performance.csv")

g<-ggplot(data=stats)+
  geom_errorbar(aes(x=train_n,ymin=meanr-sd,ymax=meanr+sd,color=as.factor(sample)),size=1.4,width=0.15)+
  geom_line(aes(x=train_n,y=meanr,color=as.factor(sample)),size=2)+
  geom_point(aes(x=train_n,y=meanr,color=as.factor(sample),shape=as.factor(sample)),size=6)+ 
  scale_color_manual(values=c("#759BFC","#68B845","#DC756E"))+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8))+
  scale_shape_manual(values=c(15,17,16))+
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
ggsave("Fig4B.pdf")

