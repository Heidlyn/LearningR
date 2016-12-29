

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
