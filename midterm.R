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
tune.out1 <- tune(svm, as.factor(activity) ~ .- subject, data=multdata, 
                 kernel="radial", 
                 ranges = list(cost = c(12.5, 13, 13.5)))
model1 <- tune.out1$best.model
summary(model1)
pro1 <- predict(model1, test_data)
write.table(pro1, "multiclass_hg0456.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote=FALSE)

pred <- predict(model, bindata)
true <- bindata$activity
table(pred, true)

pred1 <- predict(model1, multdata)
true1 <- multdata$activity
table(pred1, true1)

