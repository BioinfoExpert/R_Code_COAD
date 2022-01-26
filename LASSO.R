
options(stringsAsFactors = F)
options(warn = -1)

# packages

install.packages("glmnet")
library(glmnet)
install.packages("pROC")
library(pROC)

# always read in data and check data first
data<-read.csv("data.csv",header=T,row.names=1)
str(data)

set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))

train <- data[ind==1, ] #the training data set
test <- data[ind==2, ] #the test data set

# Convert data to generate input matrices and labels:
x <- as.matrix(train[, 1:9])
y <- train[, 10]

fitCV <- cv.glmnet(x, y, family = "binomial",
                   type.measure = "class",
                   nfolds = 5)
plot(fitCV)

lambda.min=fitCV$lambda.min
lambda.min

# get the coef
coef.min = coef(fitCV, s = "lambda.min") 
coef.min

# check the coef
fit <- glmnet(x, y, family = "binomial", alpha = 1) # make sure alpha = 1
plot(fit, xvar="lambda",label = TRUE)
abline(v = log(lambda.min,10), lty = 3,
       lwd = 2,
       col = "black")



predCV.train <- predict(fitCV, newx = as.matrix(train[,1:9]),
                        s = "lambda.min",
                        type = "response")
actuals.train <- ifelse(train$class == "malignant", 1, 0)

predCV.test  <- predict(fitCV, newx = as.matrix(test[,1:9]),
                        s = "lambda.min",
                        type = "response")
actuals.test <- ifelse(test$class == "malignant", 1, 0)


## plot ROC 
x1<-plot.roc(actuals.train,predCV.train,
             smooth=F,
             lwd=2,
             ylim=c(0,1),
             xlim=c(1,0),
             legacy.axes=T,
             main="",
             col="red")
x2<-plot.roc(actuals.test,predCV.test,
             smooth=F,
             add=T,
             lwd=2,
             ylim=c(0,1),
             xlim=c(1,0),
             legacy.axes=T,
             main="",
             col="seagreen3")
# check ROC
x1[["auc"]]
x2[["auc"]]

# add figure legend
legend.name <- c(paste("Train:AUC",sprintf("%.4f",x1[["auc"]])),
                 paste("Test:AUC",sprintf("%.4f",x2[["auc"]])))
legend("bottomright", 
       legend=legend.name,
       lwd = 2,
       col = c("red","seagreen3"),
       bty="n")
