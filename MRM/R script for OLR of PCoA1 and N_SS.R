library(vegan)
library(ggplot2)
library(psych)
arg=read.csv(file.choose(),check.names = FALSE, header = TRUE,row.names=1) #读第一行为样品第一列为phage的population re
arg=t(arg)
arg.bray<-vegdist(arg,na.rm = TRUE)
spe.b.pcoa<-cmdscale(arg.bray, k=(nrow(arg)-3),eig=TRUE) 
options(max.print=1000000)
spe.b.pcoa
pcoa1<-data.frame(spe.b.pcoa$points[,1])  #计算各个pcoa 1
pcoa1

ml<-read.csv(file.choose(),header = TRUE) #读取for plot
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

#计算线性R2
reg<-lm(treat~pcoa1,data = ml[2:9,])  
summary(reg)

#计算spearman相关性（整体）
cor2<-corr.test(ml[10:24,1:2],use="complete",method="spearman",adjust="fdr",alpha=0.05)
cor2$r
cor2$p 

cor2<-corr.test(ml[1:9,1:2],use="complete",method="spearman",adjust="fdr",alpha=0.05)
cor2$r
cor2$p 

bacteria=read.csv(file.choose(),check.names = FALSE, header = TRUE,row.names=1) #读第一行为样品第一列为phage的population re
bacteria=t(bacteria)
bacteria.bray<-vegdist(bacteria,method='bray',na.rm = TRUE)
bacteria.bray.pcoa<-cmdscale(bacteria.bray, k=(nrow(bacteria)-3),eig=TRUE)
options(max.print=1000000)
bacteria.bray.pcoa
pcoa1.bacteria<-data.frame(bacteria.bray.pcoa$points[,1])  #计算各个pcoa 1
pcoa1.bacteria


bac.pha.p1<-read.csv(file.choose(),header = TRUE) #读取for plot
head(bac.pha.p1)
bac.pha.p1.p<-ggplot(bac.pha.p1,aes(bacteria.bray.pcoa,phage.bray.pcoa))+
  geom_point(aes(color=treat),size=3)+
  geom_smooth(method = 'lm',color='red')+
  scale_color_brewer(palette ='Set1')+
  #scale_fill_brewer(palette ='Set1')+
  #facet_grid(.~group,scales = 'free')+
  #labs(y=paste("PCoA 1 (",format(100* spe.b.pcoa$eig[1] / sum(spe.b.pcoa$eig), digits=4), "%)", sep=""))+
  theme_bw(base_size = 18)+
  theme(#legend.position = 'none',
        panel.grid.minor = element_blank(),
        #axis.title.x = element_blank(),
        axis.text = element_text(color = 'black'))
bac.pha.p1.p
#计算线性R2
reg.all<-lm(bacteria.bray.pcoa~phage.bray.pcoa,data = bac.pha.p1[,1:2])  
summary(reg.all)

#计算spearman相关性（整体）
cor2.all<-corr.test(bac.pha.p1[,1:2],use="complete",method="spearman",adjust="fdr",alpha=0.05)
cor2.all$r
cor2.all$p 



