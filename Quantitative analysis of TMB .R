
# Load data
setwd("")
df=read.table("TMB inputdata.txt",header = T,sep = "\t",row.names = 1)
library(ggpubr)
#data("ToothGrowth")
#df <- ToothGrowth
df$Expression=log10(df$Expression+1)

head(df, 4)
group=levels(factor(df$Subtype))
comp=combn(group,2)
my_comparisons=list()
for(j in 1:ncol(comp)){my_comparisons[[j]]<-comp[,j]}
#»æÖÆboxplot
boxplot=ggboxplot(df, x="Subtype", y="Expression", color="Subtype",
                  xlab="Subtype",
                  ylab="TMB",
                  title=paste0("Cancer: ","Subtype"),
                  add = "jitter")+ 
  stat_compare_means(comparisons = my_comparisons)
pdf(file=paste0("Subtype.pdf"),width=5.5,height=5)
print(boxplot)
dev.off()
