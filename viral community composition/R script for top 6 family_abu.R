library(ggplot2)
library(reshape2)
library(RColorBrewer)

library(ggforce)
library(Rmisc)

data_top6<-read.csv(file.choose(),header = TRUE,check.names = 0)
head(data_top6)

#m_top6<-melt(data_top6,measure.vars = c('Herpesviridae', 'Myoviridae', 'Podoviridae', 'Circoviridae'),
 #            variable.name = "Classification",value.name = "Relative_abundance")
m_top6<-melt(data_top6,measure.vars = c( 'Siphoviridae','Microviridae'),
                   variable.name = "Classification",value.name = "Relative_abundance")
head(m_top6)#summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
#m1 = ddply(m,.(Treatment,Classification),summarize,
#           mean = round(mean(Relative_abundance),2),
#           sd = round(sd(Relative_abundance)),2)

m1_top6 <- summarySE(m_top6, measurevar="Relative_abundance", groupvars=c("Treatment","Classification"))
head(m1_top6)
m1_top6$Treatment<- factor(m1_top6$Treatment,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))


head(m1_top6)
mp_top6<-ggplot(m1_top6,aes(Treatment,Relative_abundance,fill=Classification))+
  geom_bar(width=0.8,stat = 'identity')+
  geom_errorbar(aes(ymin=Relative_abundance-se, ymax=Relative_abundance+se),
                colour="black", width=.3)+
  #scale_fill_manual(values = c(brewer.pal(12,"Paired"),brewer.pal(9,"Set3")))+
  facet_grid(.~Classification, scales = "free", space = "free")+
  scale_fill_brewer(palette = 'Spectral')+
  scale_y_continuous(expand = c(0.01,0))+
  ylab('Relative abundance')+
  theme_bw(base_size = 18)+
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        #legend.position = 'none'
        )

mp_top6