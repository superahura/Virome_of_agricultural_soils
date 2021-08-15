library("vegan")
library("ggplot2")
library(ggrepel)
# calculate nmds 记得改stress
data_all_mds<-read.csv(file.choose(),check.names= FALSE,row.names=1)
spe<-t(data_all_mds)

dist<-vegdist(spe,method="bray")     #将otu换成bray矩阵
nmds<-monoMDS(dist)                  #计算nmds

plot(nmds)                          #画图
nmds                                #获取stress
# read group
G<-read.csv(file.choose(),row.names=1)
# plot
s<-scores(nmds,choices=c(1,2))
s1<-data.frame(s,G)

s1$group<-factor(s1$group,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))


mp_all_mds<-ggplot(s1,aes(x=MDS1,y=MDS2,color=group))+
  geom_point(aes(color=group),size=5)+
  #scale_colour_brewer(palette="Paired")+
  #scale_color_manual(values = c('black','#2179B4','#35A12E','#E31C1E'))+
  scale_color_brewer(palette = 'Set1')+
  #scale_x_continuous(breaks = seq(-2,2,1),limits = c(-2.2,2))+#"#919DBA","#DB8282"
  #scale_y_continuous(breaks = seq(-2,2,1),limits = c(-2.2,2))+
  #stat_ellipse(type = "t",linetype=2)+
  labs(x='NMDS1',y='NMDS2')+
  #geom_label_repel(data=s1,aes(MDS1,MDS2,label=row.names(s1)))+
  annotate("text",x= -0.8,y= -1.5,size=4,label="Stress = 0.135")+ #R =; p = ; Stress =;Anosim= 
  theme_bw(base_size = 18)+
  theme(axis.text=element_text(colour="black"),
        legend.position = 'right',
        legend.background = element_blank())
mp_all_mds
#6.5*5
