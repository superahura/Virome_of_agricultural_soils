library(vegan)
all=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)  ##Gourp ????Ʒ??˳??Ҫ??OTU tableһ?£??????????Ǵ????ģ?
all=t(all)
all.dist <- vegdist(all) 
#adonis检验pcoa显著性
adonis(all.dist ~ ., group, perm=200)
adonis(all ~ ., group, perm=999,method="bray")
attach(group)
#ANOSIM检验nmds显著性
all.ano <- anosim(all.dist, group$group)##
summary(all.ano)
#结果图
plot(all.ano)
all.mrpp <- mrpp(all, group$group, distance="bray")
all.mrpp
