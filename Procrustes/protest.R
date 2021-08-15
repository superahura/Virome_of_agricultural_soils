library(ggplot2)
library(vegan)
library(labdsv)
library(MASS)
library(RColorBrewer)
bc<-read.csv(file.choose(),row.names=1,check.names=FALSE)
vOTU<-read.csv(file.choose(),row.names=1,check.names=FALSE)
group<-read.csv(file.choose(),row.names=1,check.names=FALSE)

bc.dis<-dsvdis(t(bc),'bray/curtis')
vOTU.dis<-dsvdis(t(vOTU),'bray/curtis')

pro<-protest(pco(bc.dis,k=23),pco(vOTU.dis,k=23),permutation=9999)
#k=n-1
pro
eigen <- sqrt(pro$svd$d)#
percent_var <- signif(eigen/sum(eigen), 4)*100

bc_pro <- data.frame(pro$X)
vOTU_pro <- data.frame(pro$Yrot)
bc_pro$sampleid <- rownames(otu_pro)
bc_pro$type <- "Bacteria"
vOTU_pro$sampleid <- rownames(gene_pro)
vOTU_pro$type <- "Virus"

colnames(vOTU_pro) <- colnames(bc_pro)

pval <- signif(pro$signif, 1)

p_data <- rbind(bc_pro, vOTU_pro)
head(p_data)
p_data<-data.frame(p_data, group)
p_pro<- ggplot(p_data,aes(fill=group))+
  # geom_point(aes(x = Dim1, y = Dim2,shape=type,color=group),size=3) +
  geom_point(aes(x = Dim1, y = Dim2,shape=type),size=3) +
  labs(x = paste0("PC 1 (",percent_var[1],"%)"),
       y = paste0("PC 2 (",percent_var[2],"%)"))+
  scale_shape_manual(values = c(21,24)) +
  scale_color_brewer(palette = 'Paired') +
  scale_fill_brewer(palette = 'Paired') +
  #scale_fill_manual(values = c("#B2DF8A","#FB9A99")) +
  theme_bw(base_size = 18) +
  geom_line(aes(x= Dim1, y=Dim2, group=sampleid), 
            col = "#4D4D4D", alpha = 0.8) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(color = 'black'),
        legend.background = element_blank(),
        legend.margin = margin(0,0,0,0))
p_pro

mantel(bc.dis,vOTU.dis,method="spear",permutation=9999)
