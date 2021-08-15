#reand environmental factors

site_env <- read.csv(file.choose(), row.names = 1, check.names = FALSE)

env <- site_env[,1:14] #soil property 
env <- site_env[,15:20]# select bio factor

#Evaluate multicollinearity among soil properties or bio factors

library(Hmisc)
env_varclus <- varclus(as.matrix(env), imilarity = 'spearman', method = 'complete')   #calculate spearman coorelation between variables
plot(env_varclus)
env_varclus   # p2 >0.7,removed from varclus results

##caluculate distance
#read vOTU data
vOTU<- read.csv(file.choose(),row.names = 1, check.names = FALSE) #first colum=otu name
vOTU <- data.frame(t(vOTU)) 

 #Calculate the Bray-Curtis variance of species composition between communities
#After converting  to similarity by  1-Bray-Curtis,  it is converted by ln
library(vegan)
comm_dis <- vegdist(spe, method = 'bray')  #first colum=sample name  
comm_sim <- log(1 - comm_dis, exp(1))

#each factor is calculted as Euclidean distance measurement
As <- vegdist(env['As'], method = 'euclidean')  
Cd <- vegdist(env['Cd'], method = 'euclidean') 
Cr <- vegdist(env['Cr'], method = 'euclidean') 
Pb <- vegdist(env['Pb'], method = 'euclidean') 
pH <- vegdist(env['pH'], method = 'euclidean') 
TC <- vegdist(env['TC'], method = 'euclidean') 
TS <- vegdist(env['TS'], method = 'euclidean') 
OM <- vegdist(env['SOM'], method = 'euclidean') 

Actinobacteria<-vegdist(env['Actinobacteria'], method = 'euclidean')
Bacteroidetes<-vegdist(env['Bacteroidetes'], method = 'euclidean')
Chloroflexi<-vegdist(env['Chloroflexi'], method = 'euclidean')
Firmicutes<-vegdist(env['Firmicutes'], method = 'euclidean')
Planctomycetes<-vegdist(env['Planctomycetes'], method = 'euclidean')
Proteobacteria<-vegdist(env['Proteobacteria'], method = 'euclidean')

#Compare the relative importance of each variable by regression coefficient

library(MuMIn)
As_std <- stdize(As)  #standardizing the distance measure of the environment
Cd_std <- stdize(Cd)
Cr_std <- stdize(Cr)
Pb_std <- stdize(Pb)
pH_std <- stdize(pH)
TC_std <- stdize(TC)
TS_std <- stdize(TS)
OM_std <- stdize(OM)

Actinobacteria_std <- stdize(Actinobacteria)
Bacteroidetes_std <- stdize(Bacteroidetes)
Chloroflexi_std <- stdize(Chloroflexi)
Firmicutes_std <- stdize(Firmicutes)
Planctomycetes_std <- stdize(Planctomycetes)
Proteobacteria_std <- stdize(Proteobacteria)

#MRM analysis

library(ecodist)

#Estimate p values based on 9999 permutations

#bio factors
fit_MRM1<-MRM(comm_sim~Bacteroidetes_std+Planctomycetes_std+Chloroflexi_std+Firmicutes_std+Actinobacteria_std,nperm = 9999,method="linear")
fit_MRM1 #factor with P<0.05 is removed from MRM model, then perform MRM again

#  MRM for soil property
fit_MRM2 <- MRM(comm_sim~+As_std+Cd_std+Pb_std+TS_std+OM_std+TC_std+Cr_std+pH_std,
    nperm = 9999, method = 'linear')
fit_MRM2

#plot(MRM result)

library(ggplot2)
data_coeffi<-read.csv(file.choose(),header = TRUE,check.names = 0)
head(data_coeffi)
data_coeffi$group<-factor(data_coeffi$group,levels = c('env','bio'))
data_coeffi$env<-factor(data_coeffi$env,levels = c('Cd','Cr',"SOM","TC",'pH','Actinobacteria','Firmicutes'))

#facet plot
head(data_coffi)
mp_coeffi<-ggplot(data_coeffi,aes(env,coffi,fill=env))+
  geom_bar(width=0.8,stat = 'identity')+
  facet_grid(.~group, scales = "free", space = "free")+
  scale_fill_brewer(palette = 'Paired')+
  ylab('Coefficient')+
  theme_bw(base_size = 18)+
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        axis.title.x = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
  )

mp_coeffi
