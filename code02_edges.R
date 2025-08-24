library(tidyverse)
source("functions.R")

d<-read_csv("data06_summary.csv") %>% filter(set!="empty")

edges_res<-tribble(~"add",~"eff",~"func",~"func_bg",~"set",~"bg",~"nspecies",~"p")
for(i in unique(d$set)){
  select_data<-filter(d,set==i)
  subsets<-subset(i)
  for(j in unique(subsets)){
    subsets_data<-filter(d,set==j) 
    if(nrow(subsets_data)>=1){
      edges_res<-edges_res %>% add_row(
        "add"=set_comp(i,j),
        "eff"=mean(select_data$res_AUC)-mean(subsets_data$res_AUC),
        "func"=mean(select_data$res_AUC),
        "func_bg"=mean(subsets_data$res_AUC),
        "set"=i,
        "bg"=j,
        "nspecies"=select_data$nspecies[1],
        "p"=t.test(select_data$res_AUC,subsets_data$res_AUC)$p.value
      )
    }
  }
}

write_csv(edges_res,"data07_res_edges.csv")


edges_gfp<-tribble(~"add",~"eff",~"func",~"func_bg",~"set",~"bg",~"nspecies")
for(i in unique(d$set)){
  select_data<-filter(d,set==i)
  subsets<-subset(i)
  for(j in unique(subsets)){
    subsets_data<-filter(d,set==j) 
    if(nrow(subsets_data)>=1){
      edges_gfp<-edges_gfp %>% add_row(
        "add"=set_comp(i,j),
        "eff"=mean(select_data$gfp_AUC)-mean(subsets_data$gfp_AUC),
        "func"=mean(select_data$gfp_AUC),
        "func_bg"=mean(subsets_data$gfp_AUC),
        "set"=i,
        "bg"=j,
        "nspecies"=select_data$nspecies[1]
      )
    }
  }
}

write_csv(edges_gfp,"data08_gfp_edges.csv")

functint<-tribble(~"from",~"to",~"dif",~"p")
for(i in 1:8){
  for(j in (1:8)[-i]){
    sub<-edges_res %>% filter(add==stnames[i]) %>% rowwise()
    functint<-functint %>% add_row(
      "to"=stnames[i],
      "from"=stnames[j],
      "dif"=mean(filter(sub,stnames[j] %in% set_to_namelist(bg))$eff)-mean(filter(sub,stnames[j] %!in% set_to_namelist(bg))$eff),
      "p"=t.test(filter(sub,stnames[j] %in% set_to_namelist(bg))$eff,filter(sub,stnames[j] %!in% set_to_namelist(bg))$eff)$p.value                                                                   
    )
  }
}
write_csv(functint,"data09_functint.csv")

shapley<-edges_res %>% filter(add!="Cab074") %>% group_by(add,nspecies) %>% summarize(mean=mean(eff)) %>% summarize(shapley=mean(mean)) %>%
  mutate(add=factor(add,levels=stnames[-9]))
write_csv(shapley,"data10_shapley_value.csv")



shapleyint<-tribble(~"from",~"to",~"dif")
for(i in 1:8){
  for(j in (1:8)[-i]){
    pres<-edges_res %>% rowwise() %>% filter(add==stnames[i],stnames[j] %in% set_to_namelist(bg)) %>%
      group_by(nspecies) %>% summarize(mean=mean(eff)) 
    abs<-edges_res %>% rowwise() %>% filter(add==stnames[i],stnames[j] %!in% set_to_namelist(bg)) %>%
      group_by(nspecies) %>% summarize(mean=mean(eff)) 
    shapleyint<-shapleyint %>% add_row(
      "to"=stnames[i],
      "from"=stnames[j],
      "dif"=mean(pres$mean)-mean(abs$mean)
      )
  }
}
