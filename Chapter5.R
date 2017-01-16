

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
#5-2 决策树算法 P78

#读入数据
data=read.csv("./data/sales_data.csv")[,2:5]

#数据命名
colnames(data)<-c("x1","x2","x3","result")

#计算一列数据的信息熵
calculateEntropy <- function(data){
  t <- table(data)    
  sum <- sum(t)       
  t <- t[t!=0]        
  entropy <- -sum(log2(t/sum)*(t/sum))
  return(entropy)
}

#计算两列数据的信息熵
calculateEntropy2 <- function(data){
  var <- table(data[1])
  p <- var/sum(var)
  varnames <- names(var)
  array <- c()
  for(name in varnames){
    array <- append(array,calculateEntropy(subset(data,data[1]==name,select=2)))
  }
  return(sum(array*p))
}

buildTree <- function(data){
  if(length(unique(data$result)) == 1){
    cat(data$result[1])
    return()
  }
  if(length(names(data)) == 1){
    cat("...")
    return()
  }
  entropy <- calculateEntropy(data$result)  
  labels <- names(data)
  label <- ""
  temp <- Inf
  subentropy <- c()
  for(i in 1:(length(data)-1)){
    temp2 <- calculateEntropy2(data[c(i,length(labels))])
    if(temp2 < temp){         
      temp <- temp2          
      label <- labels[i]      
    }
    subentropy <- append(subentropy,temp2)  
  }
  cat(label)
  cat("[")
  nextLabels <- labels[labels != label]
  for(value in unlist(unique(data[label]))){
    cat(value,":")
    buildTree(subset(data,data[label]==value,select=nextLabels))
    cat(";")
  }
  cat("]")
}

#构建分类树
buildTree(data)


#########################################
#5-3 BP神经网络算法预测

##读入数据
Data <- read.csv('./data/sales_data.csv')[, 2:5]

##数据命名
colnames(Data) <- c('x1', 'x2', 'x3', 'y')

####最终模型
library(nnet)
modell <- nnet(y~., data = Data, size = 6, decay = 5e-4, maxit = 1000)
pred <- predict(modell, Data[,1:3], type = 'class' )
p <- sum(as.numeric(pred==Data$y))/nrow(Data)
table(Data$y, pred)
prop.table(table(Data$y, pred), 1)



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


#########################################
#5-5 Apriori 算法 P99

library(arules)

##读入数据
#data <- read.table()
tr <- read.transactions('./data/menu_orders.txt', format = 'basket', sep = ',')
summary(tr)
inspect(tr)

##支持度0.2 置信度0.5
rules0 <- apriori(tr, parameter = list(support=0.2,confidence=0.5))
rules0
inspect(rules0)


#########################################






