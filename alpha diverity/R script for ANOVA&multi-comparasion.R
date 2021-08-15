library(car)
library(agricolae)
#import data that including all variables，analyze variables one by one，revise data$(Variable) 4 times
#first column=treatment (CK,CK,CK...)

data_anova<-read.csv(file.choose(),header = TRUE)
head(data_anova)
#set treat as factor
data_anova$treat <- as.factor(data_anova$treat)
class(data_anova$treat)
#F-test
nom <- bartlett.test(data_anova$Simpson~data_anova$treat,data =data_anova) #1. bartlett.test 1 data$
nom
nom<-leveneTest(data_anova$Simpson~data_anova$treat,data_anova = data_anova) #2. levene.test 2 data$
nom #p.value > 0.05, it's ok for downstream analysis
 
#ANOVA, significant
oneway<-aov(data_anova$Simpson ~data_anova$treat,data = data_anova) #3 data$
anova(oneway) 

#multi-comparison
#LSD(Fisher’s Least Significant Difference)
out <- LSD.test(oneway,"data_anova$treat",p.adj="BH")
out
mar<-out$groups
rownamemar<-row.names(mar)
newmar<-data.frame(rownamemar,mar$`data_anova$Simpson`,mar$groups) #4 data$
sort<-newmar[order(newmar$rownamemar),]
#sort row name for data frame to ensure the Correspondence of mean, sd and group
rowname<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-sort$mar.groups
plotdata<-data.frame(rowname,mean,sd,marker)
plotdata