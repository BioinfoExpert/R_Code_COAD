#ConsensusClusterPlus  TCGA and CGGA RNAseq from https://www.cancer.gov/tcga and http://www.cgga.org.cn/
rm(list=ls())
setwd("")
options(scipen = 1);
print(109000000);
rt=read.table("TCGA inputdata.txt",header=T,sep="\t",row.names = 1)
d=round(rt,2)
d=as.matrix(d)

#We generally refer to the above chip representation data as nothing but normalization

mads=apply(d,1,mad)# The absolute median difference of MAD (x) takes the median of D data as row (1)

d=d[rev(order(mads))[1:1471],]
#get rid of the first 5,000
d = sweep(d,1, apply(d,1,median,na.rm=T))
colors <- colorRampPalette(c("white", "red"))(5)
library(ConsensusClusterPlus)
title=tempdir()
class(d)
results = ConsensusClusterPlus(d,maxK=6,reps=50,pItem=0.8,pFeature=1,
                               clusterAlg="hc",distance="pearson",plot="png",tmyPal =colors )

#The number of clusters K= 2,3,4....6, 80% of the samples were sampled by the re-sampling scheme. After multiple sampling, a stable and reliable subgroup classification was found.

## The samples with class tags are then used to look for tagged genes that can be classified into the samples.Tag genes can be found by using PAM method.¡£

#results[[2]] is theresults result of k=2
results[[4]][["consensusMatrix"]][1:5,1:5]
results[[2]][["consensusTree"]]
#Check the sample classification of k= ""
results[[4]][["consensusClass"]]
c=write.table(results[[4]][["consensusClass"]],"2.txt",sep="\t")
icl = calcICL(results,title=title,plot="png")

icl[["clusterConsensus"]]
icl[["itemConsensus"]][1:5,]
#Extract the list of results
c=results[[2]]$consensusClass
class(c)
c=as.data.frame(c)
b=write.table(icl,"2.txt",sep="\t")
