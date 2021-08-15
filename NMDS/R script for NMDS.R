library("vegan")
library("ggplot2")
library(ggrepel)

# calculate nmds 

data_all_mds<-read.csv(file.choose(),check.names= FALSE,row.names=1)
spe<-t(data_all_mds)
dist<-vegdist(spe,method="bray")     #calculate bray martrix
nmds<-monoMDS(dist)                  #calculate nmds
plot(nmds)                          
nmds                                #get stress value

# read group
G<-read.csv(file.choose(),row.names=1)
# plot
s<-scores(nmds,choices=c(1,2))
s1<-data.frame(s,G)
s1$group<-factor(s1$group,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))

mp_all_mds<-ggplot(s1,aes(x=MDS1,y=MDS2,color=group))+
  geom_point(aes(color=group),size=5)+ 
  scale_color_brewer(palette = 'Set1')+
  labs(x='NMDS1',y='NMDS2')+
  annotate("text",x= -0.8,y= -1.5,size=4,label="Stress = 0.135")+ #Anosim: R= ; p = ; Stress = see above;
  theme_bw(base_size = 18)+
  theme(axis.text=element_text(colour="black"),
        legend.position = 'right',
        legend.background = element_blank())
mp_all_mds

#ANOSIM test for NMDS significance
all.ano <- anosim(all.dist, group$group)  #get R and p valure 
summary(all.ano)