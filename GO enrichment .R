rm(list = ls())
setwd("")
library(clusterProfiler)
library(org.Hs.eg.db)
gene.df<-read.table('blue.txt',header = T,stringsAsFactors = F,check.names = F,sep='\t')
gene=gene.df$ID
gene.df <- bitr(gene, fromType = "SYMBOL",
                toType = c("ENSEMBL", "ENTREZID"),
                OrgDb = org.Hs.eg.db)
head(gene.df)



erich.go.BP = enrichGO(gene = gene.df$ENTREZID,
                       OrgDb = org.Hs.eg.db,
                       ont = "BP",
                       pAdjustMethod= "BH",
                       pvalueCutoff = 0.05,
                       qvalueCutoff = 0.05,
                       readable=T)
                  

dotplot(erich.go.BP)
erich.go.BP=data.frame(erich.go.BP)
class(erich.go.BP)
b=write.table(erich.go.BP,"BP.txt",sep="\t") 

erich.go.CC = enrichGO(gene = gene.df$ENTREZID,
                       OrgDb = org.Hs.eg.db,
                       ont = "CC",
                       pAdjustMethod= "BH",
                       pvalueCutoff = 0.05,
                       qvalueCutoff = 0.05,
                       readable=T)

barplot(erich.go.CC)
dotplot(erich.go.CC)
b=write.table(erich.go.CC,"CC.txt",sep="\t") 

erich.go.MF = enrichGO(gene = gene.df$ENTREZID,
                       OrgDb = org.Hs.eg.db,
                       ont = "MF",
                       pAdjustMethod= "BH",
                       pvalueCutoff = 0.05,
                       qvalueCutoff = 0.05,
                       readable=T)

barplot(erich.go.MF)
dotplot(erich.go.MF)
b=write.table(erich.go.MF,"MF.txt",sep="\t")


erich.go.ALL = enrichGO(gene = gene.df$ENTREZID,
                        OrgDb = org.Hs.eg.db,
                        ont = "ALL",
                        pAdjustMethod= "BH",
                        pvalueCutoff = 0.05,
                        qvalueCutoff = 0.05,
                        readable=T)
b=write.table(erich.go.ALL,"ALL.txt",sep="\t")


