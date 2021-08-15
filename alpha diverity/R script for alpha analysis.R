library(vegan)
data_OTU<-read.csv(file.choose(),row.names = 1,header = TRUE,check.names = FALSE) #first column=otu name
#Calculate relative abundance
relative_abundance=function(d){
  dta_sum=apply(d,2,function(x){x/sum(x)})
}
abu<-relative_abundance(d = data_OTU)
abu<-as.data.frame(abu)
write.csv(abu,file='vOTU_abu.csv')

#calculate alpha-diversity indexes
alpha <- function(x, tree = NULL, base = exp(1)) {
 
  Richness <- rowSums(x > 0)
  Shannon <- diversity(x, index = 'shannon', base = base)
  Simpson <- diversity(x, index = 'simpson')    #Gini-Simpson 指数
  
  result <- data.frame(Richness, Shannon, Simpson)
  result
}
abu_alpha<-t(data_OTU) #first column=treatment (CK_7,CK_14,CK_24...)
alpha_all <- alpha(abu_alpha, base = exp(1))
alpha_all
write.csv(alpha_all,file='vOTU_alpha2.csv')
