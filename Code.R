library(tidytuesdayR)
library(tidyverse)
library(ggrepel)
tuesdata <- tidytuesdayR::tt_load(2022, week = 43)
tuesdata$episodes%>%data.frame()->episodesdata

#episodes
episodesdata%>%
  select(series,episode,bakers_appeared,eliminated)->data

data%>%
  group_by(series,episode)%>%
  arrange(episode, .by_group = TRUE)%>%
  data.frame()->data1

data1%>%
  group_by(series)%>%
  mutate(diff=ave(bakers_appeared, FUN = function(x) c(0, diff(-x))))->data1

colnames(data1)<-c("series","episode","bakers","eliminated","difference")

data1%>%
  transform(data1, nxt_value = c(difference[-1], NA))%>%
  select(series,episode,bakers,eliminated,nxt_value)->data21

data21$nxt_value[is.na(data21$nxt_value)] <- 0
data21

ggplot(data21,aes(x=as.factor(series),y=as.factor(episode),fill=factor(nxt_value)))+
  geom_tile(colour="white")+
  coord_equal()+
  scale_fill_brewer(palette="RdPu")+
  xlab("-------------SERIES---------------")+
  ylab("-------------EPISODES--------------")+
  labs(fill="NUMBER OF ELIMINATIONS: ")+
  theme(plot.background=element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        legend.background = element_rect(fill="black"),
        legend.text = element_text(colour="white",face = "bold",margin = margin(l=5)),
        legend.title = element_text(colour="white",face = "bold",size=12,margin = margin(b=5)),
        legend.box.margin = unit(c(1,2,1,2),"cm"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  labs(title="THE GREAT BRITISH BAKEOFF ELIMINATIONS",
       caption = "Data via Tidy Tuesday| Analysis and design: @annapurani93")->heatmap
   
ggsave("heatmap.png",heatmap,width=9.6,height=5.5)
