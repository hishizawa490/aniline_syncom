library(tidyverse)
source("functions.R")

colors<-c("#77A755","#FFB300","#EB848F","#0150BD","#00B3FF","#3E7EB3","#2A597B","#F60000","#C494BA","#FF6E01",
          "#FFFB59","#5E988A","#9685D3","#333333","#FF33CC","#C17B35","#BB00BB","gray")

asvtable<-read_csv("data13_restored_community.csv") %>% select(-c(2:9)) 
top_asvs<-asvtable %>% slice(1:17)
others<-asvtable %>% slice(19:n()) %>% select(-ID) %>% summarise(across(everything(),sum)) %>%
  mutate(ID="Others") %>% select(ID, everything())
asvtable<-bind_rows(top_asvs,others)


asv_long <- asvtable %>%
  pivot_longer(-ID, names_to = "Sample", values_to = "Abundance")

g<-ggplot(asv_long, aes(x = Sample, y = Abundance, fill = ID)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_minimal() +
  theme(
    aspect.ratio=0.8,
    axis.text = element_text(color="black",size=14),
    axis.title=element_blank(),
    legend.text=element_blank(),
    legend.title=element_blank()
    )
win.graph(10,10)
g

ggsave("FigS1.pdf")
