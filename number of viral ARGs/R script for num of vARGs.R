# vARGs composition
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plyr)
data_num_vA<-read.csv(file.choose(),header = TRUE,check.names = 0)
head(data_num_vA)
m_all<-melt(data_num_vA,measure.vars = c( 'Tetracycline','Others','Multidrug','Aminoglycoside','Vancomycin','MLS','Chloramphenicol','Trimethoprim'),
            variable.name = 'Classification',value.name = 'num')
head(m_all)
m1_all <- ddply(m_all,.(Treatment,Classification),summarize,mean=round(mean(num),2),sd=round(sd(num),2))
head(m1_all)

#m1_all<- summarySE(m_all, measurevar="num", groupvars=c("Treatment","Classification"))
#head(m1_all)

m1_all$Treatment<- factor(m1_all$Treatment,levels = c("CK","0.5N","1N","0.5SS","1SS","2SS","4SS","1CM"))

m1_all$Classification<- factor(m1_all$Classification,levels = c( 'Tetracycline','Multidrug','Others','Aminoglycoside','Vancomycin','MLS','Chloramphenicol','Trimethoprim'))
m1_all_n <- ddply(m1_all,.(Treatment),transform,label_y = cumsum(mean))
mp_all<-ggplot(m1_all_n,aes(Treatment,mean,fill=Classification))+
  geom_bar(width=0.62,stat = 'identity')+
  #geom_errorbar(aes(ymin=label_y-se, ymax=label_y+se),
              #  colour="black", width=.3)+
  scale_fill_brewer(palette = "Paired")+
  expand_limits(y = c(0, 9))+
  #scale_fill_manual(values = c(brewer.pal(12,"Paired"),brewer.pal(9,"Set3")))+
  scale_y_continuous(expand = c(0.01,0))+
  #ylim(0,14)+
  #annotate("text", x = 3, y = 14, label = "M")+
  theme_bw(base_size = 18)+
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'right')
mp_all
