

getwd()

setwd("E:\\R\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter5\\chapter5\\示例程序")

setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter5\\chapter5\\示例程序")

ls() 
rm(list = ls())

#########################################
#5-1 P 72 Logistic 回归

Data <- read.csv("./data/bankloan.csv")[2:701,]

#数据命名
colnames(Data) <- c("x1","x2","x3","x4","x5","x6","x7","x8","y")

#回归模型
glm <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8, family = binomial(link = logit), data = Data)
summary(glm)

#逐步寻优
logit.step <- step(glm, direction = "both")
summary(logit.step)

#前向选择
logit.step <- step(glm, direction = "forward")
summary(logit.step)

#后向选择
logit.step <- step(glm, direction = "backward")
summary(logit.step)



#########################################
#5-4 K-means 聚类算法 P92

#读取数据
Data <- read.csv('./data/consumption_data.csv', header = T)[,2:4]  #
km <- kmeans(Data, centers = 3) #, iter.max = 10
print(km)

ksize <- km$size/sum(km$size)

#数据分组
aaa <- data.frame(Data, km$cluster)
Data1 <- Data[which(aaa$km.cluster == 1), ]
Data2 <- Data[which(aaa$km.cluster == 2), ]
Data3 <- Data[which(aaa$km.cluster == 3), ]

par(mfrow = c(1,3))
plot(density(Data1[,1]), col = 'red', main = 'R')
plot(density(Data1[,2]), col = 'red', main = 'F')
plot(density(Data1[,3]), col = 'red', main = 'M')

par(mfrow = c(1,3))
plot(density(Data2[,1]), col = 'red', main = 'R')
plot(density(Data2[,2]), col = 'red', main = 'F')
plot(density(Data2[,3]), col = 'red', main = 'M')

par(mfrow = c(1,3))
plot(density(Data3[,1]), col = 'red', main = 'R')
plot(density(Data3[,2]), col = 'red', main = 'F')
plot(density(Data3[,3]), col = 'red', main = 'M')

###自己写的测试
plot((Data[,3]))
Data1.Rmean <- mean(Data1$R)
Data1.Fmean <- mean(Data1$F)
Data1.Mmean <- mean(Data1$M)
print(Data1.Rmean)


