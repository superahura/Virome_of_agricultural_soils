library(pheatmap)
x<-read.csv(file.choose(),header = T,row.names = 1,check.names = 0)
pheatmap(x,cluster_rows = TRUE,cluster_cols = FALSE,show_rownames = 1,border_color=NA,
         cellwidth = 10,cellheight =5,fontsize_row =5,fontsize_col = 5,
         color = colorRampPalette(c(rev(c("#CD2626","#E79797","#FFFFFF","#7B7BBD","#000080"))))(100))