require(ISLR)
library(dplyr)
library(MASS)
library(e1071)

data <- as.data.frame(read.table("training_data.txt", header = TRUE))
test_data <- as.data.frame(read.table("test_data.txt", header = TRUE))

bindata <- data
bindata$activity[bindata$activity<4]<-1
bindata$activity[bindata$activity>=4]<-0
tune.out <- tune(svm, as.factor(activity) ~ ., data=bindata[,-1], kernel="linear", 
                 ranges = list(cost = c(0.01, 1, 10, 0.1)))
model <- tune.out$best.model
summary(model)
pro <- predict(model, test_data)
write.table(pro, "binary_hg0456.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote=FALSE)

multdata <- data
multdata$activity[multdata$activity>=7]<-7
tune.out1 <- tune(svm, as.factor(activity) ~ .- subject, data=multdata[multdata$activity<4, ], 
                 kernel="radial", 
                 ranges = list(cost = c(1, 10, 13)))
model1 <- tune.out1$best.model
summary(model1)
tune.out2 <- tune(svm, as.factor(activity) ~ .- subject, data=multdata[multdata$activity>=4, ], 
                  kernel="radial", 
                  ranges = list(cost = c(1, 10, 13)))
model2 <- tune.out2$best.model
summary(model2)
pro1 <- predict(model1, test_data)
pro2 <- predict(model2, test_data)
finalpro <- list()
for (i in 1:length(pro)) {
  if (pro[i] =="0") {
    finalpro <- append(finalpro, pro2[i])
  } else {
    finalpro <- append(finalpro, pro1[i])
  }
}
write.table(finalpro, "multiclass_hg0456.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote=FALSE)

sub <- multdata[multdata$activity>=4, ]
ori <- predict(model2, sub)
mean(ori==sub$activity)
