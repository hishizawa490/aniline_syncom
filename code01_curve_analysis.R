library(tidyverse)
library(gcplyr)
source("functions.R")

res<-read_csv("data01_resazurin.csv")
gfp<-read_csv("data02_GFP.csv")
wellinfo<-read_csv("data03_platedesign.csv") %>% rowwise() %>% mutate(nspecies=set_to_n(set))

res_long<-pivot_longer(res,cols=2:ncol(res),names_to="ID",values_to="RFU") %>% 
  left_join(wellinfo,by="ID")
gfp_long<-pivot_longer(gfp,cols=2:ncol(res),names_to="ID",values_to="RFU") %>% 
  left_join(wellinfo,by="ID")

blank_R<-res_long %>% filter(set=="S000000000") %>% group_by(Time,plate) %>% reframe(blankRFU=mean(RFU))
blank_G<-gfp_long %>% filter(set=="S000000000") %>% group_by(Time,plate) %>% reframe(blankRFU=mean(RFU))

res_long<-left_join(res_long,blank_R,by=c("Time","plate")) %>% mutate(RFU=RFU-blankRFU) %>% dplyr::select(-blankRFU)
res_long[res_long$plate==1,1]<-res_long[res_long$plate==1,1]-240
res_long[res_long$plate==2,1]<-res_long[res_long$plate==2,1]-440
res_long<-res_long %>% filter(Time>=0,Time<=3880)
res_long<-res_long  %>% arrange(ID) %>% mutate(RFU=res_long %>% 
                                                 group_by(ID) %>% reframe(RFU=RFU-RFU[1]+1) %>% select(RFU)) %>% mutate(RFU=RFU$RFU)
res_long$RFU[res_long$RFU<=1]<-1
res_long<-res_long %>% group_by(ID) %>% 
  mutate(r=calc_deriv(y=RFU,x=Time,percapita=T,blank=-1000,window_width_n=7,x_scale=60)) 

write.csv(res_long,"data04_reslong.csv")

gfp_long<-left_join(gfp_long,blank_G,by=c("Time","plate")) %>% mutate(RFU=RFU-blankRFU) %>% dplyr::select(-blankRFU)
gfp_long[gfp_long$plate==1,1]<-gfp_long[gfp_long$plate==1,1]-240
gfp_long[gfp_long$plate==2,1]<-gfp_long[gfp_long$plate==2,1]-440
gfp_long<-gfp_long %>% filter(Time>=0,Time<=3880)
gfp_long<-gfp_long  %>% arrange(ID) %>% mutate(RFU=gfp_long %>% 
                                                 group_by(ID) %>% reframe(RFU=RFU-RFU[1]+1) %>% select(RFU)) %>% mutate(RFU=RFU$RFU)
gfp_long$RFU[res_long$RFU<=1]<-1
gfp_long<-gfp_long %>% group_by(ID) %>% 
  mutate(r=calc_deriv(y=RFU,x=Time,percapita=T,blank=-1000,window_width_n=7,x_scale=60))

write.csv(gfp_long,"data05_gfp_long.csv")

stats<-wellinfo %>% left_join(
  tibble("ID"=colnames(res)[-1],
         res_long %>% group_by(ID) %>% summarise(res_AUC=auc(x=Time,y=RFU,neg.rm=T)) %>% dplyr::select(res_AUC),
         gfp_long %>% group_by(ID) %>% summarise(gfp_AUC=auc(x=Time,y=RFU,neg.rm=T)) %>% dplyr::select(gfp_AUC)
        ),by="ID")

stats$res_AUC<-stats$res_AUC/mean(filter(stats,nspecies==1)$res_AUC)
stats$gfp_AUC<-stats$gfp_AUC/mean(filter(stats,nspecies==1)$gfp_AUC)

write.csv(stats,"data06_summary.csv")
