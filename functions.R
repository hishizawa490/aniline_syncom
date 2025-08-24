
stnames<-c("Sph075","Pse056","Bac050","Bos046","Mic039","Sph034","Pse033","Ach031","Cab074")

colors<-c("#57BB33","#0150BD","#FFAA00","#B028F0","#00DDDD","#31691D","#EB848F","#BB1111")

'%!in%'=Negate('%in%')

set_to_namelist<-function(set){
  char<-strsplit(set,"")[[1]][-1]
  names<-c()
  for(i in 1:9){
    if(char[i]=="1"){
    names<-c(names,stnames[i])
  }}
  names
}

namelist_to_set<-function(namelist){
  set<-"S"
  for(i in 1:8){
    if(stnames[i] %in% namelist){
      set<-str_c(set,"1")
    }else{
      set<-str_c(set,"0")
    }
  }
  set<-str_c(set,"1")
  set
}

set_to_n<-function(set){
  char<-strsplit(set,"")[[1]][-1]
  sum(as.numeric(char))
}

subset<-function(set){
  char<-strsplit(set,"")[[1]][-1]
  subs<-c()
  if(sum(as.numeric(char))>=1){
  for(i in which(char==1)){
    sub<-char;sub[i]<-0
    subs<-c(subs,paste(c("S",sub),collapse=""))
  }}
  subs
}

subset_all<-function(set,exist=NA,omit_same=T,min=1,max=9){
  all<-strsplit(apply(expand.grid(rep(list(0:1), 9)),1,paste,collapse=""),"")
  char<-as.numeric(strsplit(set,"")[[1]][-1])
  subs<-c();filtered<-c()
  for(i in 1:length(all)){
    res<-as.numeric(char)-as.numeric(all[[i]])
    if(min(res)==0){
      subs<-c(subs,paste(c("S",all[[i]]),collapse=""))
    }
  }
  if(is.na(exist)==F){subs<-subs[substr(subs,which(stnames==exist)+1,which(stnames==exist)+1)==1]}
  if(omit_same==T){subs<-subs[subs!=set]}
  for(i in 1:length(subs)){
    if(sum(as.numeric(strsplit(subs[i],split="")[[1]][-1]))>=min){
      if(sum(as.numeric(strsplit(subs[i],split="")[[1]][-1]))<=max){
        filtered<-c(filtered,subs[i])
      }
    }
  }
  filtered
}


set_comp<-function(set1,set2){
  char1<-strsplit(set1,"")[[1]][-1]
  char2<-strsplit(set2,"")[[1]][-1]
  dif<-abs(as.numeric(char1)-as.numeric(char2))
  stnames[which(dif==1)]
}

subset_enrich<-function(pos,neg){
  poslist<-c();neglist<-c()
  for(i in 1:nrow(pos)){poslist<-c(poslist,subset_all(pos$set[i],"MA074",F))}
  for(i in 1:nrow(neg)){neglist<-c(neglist,subset_all(neg$set[i],"MA074",F))}
  poslist<-table(poslist);neglist<-table(neglist)
  res<-tribble(~"sub",~"nspecies",~"n_1",~"n_2",~"ratio_1",~"ratio_2")
  for(i in unique(c(names(poslist),names(neglist)))){
    res<-res %>% add_row(
      "sub"=i,
      "nspecies"=set_to_n(i),
      "n_1"=poslist[names(poslist)==i],
      "n_2"=neglist[names(neglist)==i],
      "ratio_1"=poslist[names(poslist)==i]/nrow(pos),
      "ratio_2"=neglist[names(neglist)==i]/nrow(neg)
    )
  }
  res<-res %>% rowwise()
  mutate(res,fisher_p=fisher.test(matrix(c(n_1,n_2,nrow(pos)-n_1,nrow(neg)-n_2),nrow=2))$p.value)
}




GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })




geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
  

}


marginal_cont<-function(data=NULL,target=NULL){
  d<-filter(data,add!="MA074")
  mem<-unique(d$add)
  d<-filter(d,add==target)
  perm<-permutations(n=length(mem),r=length(mem),v=mem)
  mc<-c()
  for(i in 1:nrow(perm)){
    ord<-grep(target,perm[i,])
    if(ord!=1){
    bgmem<-perm[i,1:ord-1]}else{bgmem<-NA}
    mc<-c(mc,filter(d,bg==namelist_to_set(bgmem))$eff)
  }
  mc
}



rf_model<-list()
rfrep<-function(d,train_n=2,test_n=5:9,try=100,mtry=2,sample=Inf){
  results<-tribble(~"try",~"r",~"RMSPE")
  test<-d %>% filter(nspecies %in% test_n) %>% dplyr::select(5:12,res_AUC)
  for(i in 1:try){
    set.seed(i)
    train<-d %>% filter(nspecies %in% train_n) %>% dplyr::select(5:12,res_AUC)
    train<-train %>% slice_sample(n=ifelse(sample<=1,round(nrow(train)*sample),sample))
    set.seed(i)
    suppressMessages({test<-anti_join(test,train)})
    rf_model[[i]]<-randomForest(res_AUC ~.,mtry=mtry,data=train)
    pred<-predict(rf_model[[i]],test)
    results<-results %>% add_row(
      "try"=i,
      "r"=cor(pred, test$res_AUC),
      "RMSPE"=RMSPE(pred,test$res_AUC))
  }
  list(results,rf_model[[arrange(results,r)$try[round(try/2)]]],arrange(results,r)$try[round(try/2)])
}






