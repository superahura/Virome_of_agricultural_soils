library(ggplot2)
library(reshape2)
library(RColorBrewer)
data_all<-read.csv(file.choose(),header = TRUE,check.names = 0)
head(data_all)
m_all<-melt(data_all,measure.vars = c( 'Lavidaviridae','Iridoviridae','Pithoviridae','Baculoviridae','Cruciviridae','Mimiviridae','Phycodnaviridae','Nanoviridae','Marseilleviridae','Inoviridae','Unassigned Caudovirales','Genomoviridae','Herpesviridae', 'Myoviridae', 'Podoviridae', 'Circoviridae', 'Siphoviridae','Microviridae','Unassigned'),
        variable.name = 'Classification',value.name = 'Relative_abundance')
head(m_all)

m1_all<- summarySE(m_all, measurevar="Relative_abundance", groupvars=c("Treatment","Classification"))
head(m1_all)

m1_all$Treatment<- factor(m1_all$Treatment,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))

m1_all$Classification<- factor(m1_all$Classification,levels = c( 'Unassigned','Microviridae', 'Siphoviridae', 'Circoviridae','Podoviridae','Myoviridae','Herpesviridae','Genomoviridae','Unassigned Caudovirales','Inoviridae','Marseilleviridae','Nanoviridae','Phycodnaviridae','Mimiviridae','Cruciviridae','Baculoviridae','Pithoviridae','Iridoviridae','Lavidaviridae'))
m1_all_n = ddply(m1_all,.(Treatment),transform,label_y = cumsum(Relative_abundance))
mp_all<-ggplot(m1_all_n,aes(Treatment,Relative_abundance,fill=Classification))+
  geom_bar(width=0.62,stat = 'identity')+
  geom_errorbar(aes(ymin=label_y-se, ymax=label_y+se),
                colour="black", width=.3)+
  #facet_grid(.~Classification, scales = "free", space = "free")+
  
  scale_fill_manual(values = c(brewer.pal(12,"Paired"),brewer.pal(9,"Set3")))+
  scale_y_continuous(expand = c(0.01,0))+
  theme_bw(base_size = 18)+
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'none')+
  #guides(fill=guide_legend(reverse = TRUE))
  facet_zoom(ylim = c(0,0.3),zoom.size = 1)    #
mp_all
