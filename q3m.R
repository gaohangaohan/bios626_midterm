require(ISLR)
library(dplyr)
library(MASS)
library(e1071)
library(neuralnet)

data <- as.data.frame(read.table("training_data.txt", header = TRUE))
test_data <- as.data.frame(read.table("test_data.txt", header = TRUE))

multdata <- data
multdata$activity[multdata$activity>=7]<-7
model <- neuralnet(as.factor(activity) ~ .- subject, data=multdata, hidden = c(5,3),linear.output = FALSE)
summary(model)
pro <- predict(model, test_data)
pro <- max.col(pro)
ori <- predict(model, multdata)
ori <-max.col(ori)
true <- multdata$activity
pred <- ori
table(pred, true)
mean(ori == multdata$activity)
write.table(pro, "multiclass_hg0456.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote=FALSE)

