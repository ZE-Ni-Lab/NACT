rm(list = ls())
options(stringsAsFactors = F)

## Analysis of GSE130786
load("GSE130786.Rdata")
marker_CD8T_2 = c("CD3D","CD8B","LMNA","YPEL5","XCL1","CXCR4",
                  "FAM177A1","UBE2S","TNFAIP3")
exp_CD8T_2 = exp[marker_CD8T_2,]
exp_CD8T_2 = t(na.omit(t(exp_CD8T_2)))
mean_exp_CD8T_2 = apply(exp_CD8T_2,2,mean)

boxplot_data = data.frame(CD8T_LMNA = mean_exp_CD8T_2,
                          ngroup = pd[pd$geo_accession %in% colnames(exp_CD8T_2),]$ngroup)
boxplot_data = boxplot_data[order(boxplot_data[,1]),]
perc = c(1:nrow(boxplot_data))/(nrow(boxplot_data)/100)
boxplot_data[,1] = perc
boxplot_data_130 = boxplot_data


## Analysis of GSE163882
load("data/GSE163882.Rdata")
marker_CD8T_2 = c("CD3D","CD8B","LMNA","YPEL5","XCL1","CXCR4",
                  "FAM177A1","UBE2S","TNFAIP3")
exp_CD8T_2 = exp[marker_CD8T_2,]
exp_CD8T_2 = t(na.omit(t(exp_CD8T_2)))
mean_exp_CD8T_2 = apply(exp_CD8T_2,2,mean)

boxplot_data = data.frame(CD8T_LMNA = mean_exp_CD8T_2,
                          ngroup = pd[pd$geo_accession %in% colnames(exp_CD8T_2),]$ngroup)
boxplot_data = boxplot_data[order(boxplot_data[,1]),]
perc = c(1:nrow(boxplot_data))/(nrow(boxplot_data)/100)
boxplot(perc)
boxplot_data[,1] = perc
boxplot_data = rbind(boxplot_data,boxplot_data_130)


## visualization and statistic test 
library(ggpubr)
ysegment.max = max(boxplot_data[,1])
my_comparisons <- list(c("sensitive-pre","resistant-pre"))
p=ggboxplot(boxplot_data, x = "ngroup", y = colnames(boxplot_data)[1],
            color = "ngroup", palette = c("#FBB800","#0088C4"),
            add = c("jitter"),
            order = c("resistant-pre","sensitive-pre"),
            title = colnames(boxplot_data)[1],legend = "none",
            width = 0.7,whis = 1,
            xlab = "", ylab = "Abundance")+
  stat_compare_means(method = "wilcox.test",
                     comparisons = my_comparisons,
                     label.y = c(ysegment.max))
p
