library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(Rmisc)
data_abun_Bac_A<-read.csv(file.choose(),header = TRUE,check.names = 0)
head(data_abun_Bac_A)
m_all<-melt(data_abun_Bac_A,measure.vars = c( 'Others','Multidrug','Vancomycin','Bacitracin','MLB','Fosmidomycin','Rifamycin','Quinolone','Tetracycline','Beta_Lactamase','Aminoglycoside'),
            variable.name = 'Classification',value.name = 'abundance')
head(m_all)
m1_all<- summarySE(m_all, measurevar="abundance", groupvars=c("Treatment","Classification"))
head(m1_all)

m1_all$Treatment<- factor(m1_all$Treatment,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))

m1_all$Classification<- factor(m1_all$Classification,levels = c('Aminoglycoside','Beta_Lactamase','Tetracycline','Quinolone','Rifamycin','Others','Fosmidomycin','MLB','Bacitracin','Vancomycin','Multidrug'))
m1_all_n = ddply(m1_all,.(Treatment),transform,label_y = cumsum(abundance))
mp_all<-ggplot(m1_all_n,aes(Treatment,abundance,fill=Classification))+
  geom_bar(width=0.62,stat = 'identity')+
  scale_fill_manual(values = c('grey',brewer.pal(12,"Paired")))+
  scale_y_continuous(expand = c(0.01,0))+
  theme_bw(base_size = 18)+
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'right')

mp_all