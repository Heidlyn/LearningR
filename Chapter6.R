
getwd()

setwd("E:\\R\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter6\\chapter6\\示例程序")

setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter6\\chapter6\\示例程序")

ls() 
rm(list = ls())

###################################

#6-1 数据划分 P138

##读入数据
Data <- read.csv('./data/model.csv')

##数据命名
colnames(Data) <- c('time', 'userid', 'ele_ind', 'loss_ind', 'alarm_ind', 'class')

##数据分割
set.seed(1234)

##定义序列ind,
ind <- sample(2, nrow(Data), replace=TRUE, prob = c(0.8, 0.2))

trainData <- Data[ind==1,] 
testData <- Data[ind==2,]

##数据存储
write.csv(trainData, './tmp/trainData.csv', row.names = FALSE)
write.csv(testData, './tmp/testData.csv', row.names = FALSE)


###################################
#6-2 神经网络模型 P139

##读取数据
trainData <- read.csv('./data/trainData.csv')

##将class列转为factor类型
trainData <- transform(trainData, class= as.factor(class))

####神经网络模型构建
library(nnet) 
##建立
nnet.model <- nnet(class~ele_ind+loss_ind+alarm_ind, trainData, size=10, decay=0.05 )

summary(nnet.model)

##建立混淆矩阵
confusion <- table(trainData$class, predict(nnet.model, trainData, type='class' ))
accuracy <- sum(diag(confusion)) * 100 / sum(confusion)

##保存输出结果
output_nnet.trainData <- cbind(trainData, predict(nnet.model, trainData, type='class'))
colnames(output_nnet.trainData) <- c(colnames(trainData), 'OUTPUT')
write.csv(output_nnet.trainData, './tmp/output_nnet.trainData.csv', row.names = FALSE)

##保存神经网络模型
save(nnet.model, file = './tmp/output_nnet.RData')


###################################









