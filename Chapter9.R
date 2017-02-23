
getwd()

setwd("E:\\R\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter9\\chapter9\\示例程序")

setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter9\\chapter9\\示例程序")

ls() 
rm(list = ls())

###################################

#9-1 数据抽样  P177
#读入数据
Data <- read.csv("./data/moment.csv") 


#数据命名
colnames(Data) <- c("class", "id", "R1", "G1", "B1", "R2", "G2", "B2", "R3", "G3", "B3")

#数据分割
set.seed(1234)
ind <- sample(1:2, nrow(Data), replace = TRUE, prob = c(0.8,0.2))

trainData <- Data[ind==1, ]
testData  <- Data[ind==2, ]

#数据存储
write.csv(trainData, "./tmp/trainData.csv", row.names = FALSE)
write.csv(testData,  "./tmp/testData.csv", row.names = FALSE)



###################################
#9-2 构建支持向量机模型 P178

#读取数据
trainData <- read.csv("./data/trainData.csv")
testData  <- read.csv("./data/testData.csv")

#将class列转换为factor类型
trainData <- transform(trainData, class=as.factor(class))
testData <- transform(testData, class=as.factor(class))

##支持向量机分类模型构建
#install.packages("e1071")
library(e1071)
#利用svm建立支持向量机分类模型
svm.model <- svm(class~. , trainData[, -2])
summary(svm.model)



#建立混淆矩阵
confusion <- table(trainData$class, predict(svm.model, trainData, type="class"))
accuracy <- sum(diag(confusion))*100/sum(confusion)



