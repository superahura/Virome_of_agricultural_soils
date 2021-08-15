library(zCompositions) 
library(rgr)  
vOTU<- read.csv(file.choose(), row.names = 1, check.names = 1,header = 1) #first col=vOTU name
#Zero handling
vOTU.no0 <- cmultRepl(vOTU,output="p-counts")
write.csv(vOTU.no0,file = 'vOTU.no02.csv')

# clr transform
vOTU_clr<-clr(t(vOTU.no0) )#first colum=treatment name(CK14,CK7,CK24...) for clr()
vOTU_clr<- t(vOTU_clr)  
head(vOTU_clr)
write.csv(vOTU_clr,file = 'vOTU.clr2.csv')
