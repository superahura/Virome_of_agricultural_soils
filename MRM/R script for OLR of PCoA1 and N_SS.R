library(vegan)
library(ggplot2)
library(psych)
arg=read.csv(file.choose(),check.names = FALSE, header = TRUE,row.names=1) 
arg=t(arg)
arg.bray<-vegdist(arg,na.rm = TRUE)
spe.b.pcoa<-cmdscale(arg.bray, k=(nrow(arg)-3),eig=TRUE) 
options(max.print=1000000)
spe.b.pcoa
pcoa1<-data.frame(spe.b.pcoa$points[,1])  #pcoa 1
pcoa1

ml<-read.csv(file.choose(),header = TRUE) 
head(ml)
p1<-ggplot(ml,aes(treat,pcoa1,color=group,fill=group))+
  geom_point(aes(color=group))+
  geom_smooth(aes(color=group),method = 'lm')+
  scale_color_brewer(palette ='Set1')+
  scale_fill_brewer(palette ='Set1')+
  facet_grid(.~group,scales = 'free')+
  labs(y=paste("PCoA 1 (",format(100* spe.b.pcoa$eig[1] / sum(spe.b.pcoa$eig), digits=4), "%)", sep=""))+
  theme_bw(base_size = 18)+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = 'black'))
p1

#R2
reg<-lm(treat~pcoa1,data = ml[2:9,])  
summary(reg)

#spearman
cor2<-corr.test(ml[10:24,1:2],use="complete",method="spearman",adjust="fdr",alpha=0.05)
cor2$r
cor2$p 

cor2<-corr.test(ml[1:9,1:2],use="complete",method="spearman",adjust="fdr",alpha=0.05)
cor2$r
cor2$p 
