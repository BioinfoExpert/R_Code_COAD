rm(list = ls()) 
library(survival)
setwd("")

data<-read.table("input data.txt",header=T,sep="\t")
rownames(data)=data[,1]
data=data[,-1]

data=data[,c(1,2,3,9,12)]

#data = cox_train_univar
OS.time=data$survival_time
OS=data$Vital_status

genes = colnames(data)[3:ncol(data)]

outTab = data.frame()

for(i in genes){
  
  expr = data[,i]
  
  cox = coxph(Surv(OS.time,OS)~expr,data)
  
  cox_summary = summary(cox)
  
  outTab = rbind(outTab,cbind(gene = i,HR = round(cox_summary$coefficients[,"exp(coef)"],2),
                              
                              z = round(cox_summary$coefficients[,"z"],2),
                              
                              "95%CI" = paste(round(cox_summary$conf.int[,3],2),
                                              
                                              round(cox_summary$conf.int[,4],2),sep = "-"),
                              
                              pvalue = round(cox_summary$coefficients[,"Pr(>|z|)"],50)))
  
}

b=write.table(outTab,"univariate cox.txt",sep="\t")
