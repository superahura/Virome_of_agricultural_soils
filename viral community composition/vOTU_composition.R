library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(ggforce)
library(plyr)
library(ggbreak)
data_all<-read.csv(file.choose(),header = TRUE,check.names = 0)
head(data_all)
m_all<-melt(data_all,measure.vars = c( 'Lavidaviridae','Iridoviridae','Pithoviridae','Baculoviridae','Cruciviridae','Mimiviridae','Phycodnaviridae','Nanoviridae','Marseilleviridae','Inoviridae','Genomoviridae','Herpesviridae', 'Myoviridae', 'Podoviridae', 'Circoviridae', 'Siphoviridae','Unassigned Caudovirales','Microviridae','Unassigned'),
            variable.name = 'Classification',value.name = 'Relative_abundance')
head(m_all)
m1_all <- ddply(m_all,.(Treatment,Classification),summarize,mean=mean(Relative_abundance))

head(m1_all)

m1_all$Treatment<- factor(m1_all$Treatment,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))

m1_all$Classification<- factor(m1_all$Classification,levels = c( 'Unassigned','Microviridae', 'Unassigned Caudovirales','Siphoviridae', 'Circoviridae','Podoviridae','Myoviridae','Herpesviridae','Genomoviridae','Inoviridae','Marseilleviridae','Nanoviridae','Phycodnaviridae','Mimiviridae','Cruciviridae','Baculoviridae','Pithoviridae','Iridoviridae','Lavidaviridae'))
m1_all_n <- ddply(m1_all,.(Treatment),transform,label_y = cumsum(mean))
mp_all<-ggplot(m1_all_n,aes(Treatment,mean,fill=Classification))+
  geom_bar(width=0.62,stat = 'identity')+
  labs(y='Relative abundance')+
  scale_fill_manual(values = c(brewer.pal(12,"Paired"),brewer.pal(9,"Set3")))+
  scale_y_break(c(0.34,0.99))+
  scale_y_continuous(breaks = seq(0,1,0.05),expand = c(0.01,0))+
  theme_bw(base_size = 18)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'right')
mp_all
