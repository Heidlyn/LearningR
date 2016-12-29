

getwd()

setwd("E:\\R\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter4\\示例程序")

setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter4\\示例程序")

#load("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战/图书配套数据、代码\\chapter4\\示例程序\\.RData")


ls() 
rm(list = ls())



#4-1 P43
inputfile = read.csv('./data/catering_sale.csv',he=T)
inputfile<-data.frame(sales=inputfile$'销量',date=inputfile$'日期')


#
inputfile<-inputfile[5:16,] 
inputfile

is.na(inputfile)
n <- sum(is.na(inputfile))

par(mfrow=c(1,2))
dotchart(inputfile$sales)
boxplot(inputfile$sales,horizontal = T)


inputfile$sales[5] <- NA
fix(inputfile)


inputfile$date <- as.numeric(inputfile$date)
sub <- which(is.na(inputfile$sales))
inputfile1 <- inputfile[-sub,]
inputfile2 <- inputfile[sub,]


#
result1 <- inputfile1


#
avg_sales <- mean(inputfile1$sales)
inputfile2$sales <-rep(avg_sales,n)

result2 <- rbind(inputfile1,inputfile2)


#
model <- lm(sales~date, date=inputfile)  ####报错
inputfile2$sales <- predict(model,inputfile2)



#多重插补
install.packages('mice')
library(lattice)
library(MASS)
library(nnet)
library(mice)

imp <- mice(inputfile,m=4)
fit <- with(imp, lm(sales~date, data=inputfile))
pooled <- pool(fit)
summary(pooled)
result4 <- complete(imp,action=3)



###################################
#4-2 P49

data <- read.csv('./data/normalization_data.csv',he=T)

#最大值、最小值规范化
b1 <- (data[,1]-min(data[,1]))/(max(data[,1])-min(data[,1]))
b2 <- (data[,2]-min(data[,2]))/(max(data[,2])-min(data[,2]))
b3 <- (data[,3]-min(data[,3]))/(max(data[,3])-min(data[,3]))
b4 <- (data[,4]-min(data[,4]))/(max(data[,4])-min(data[,4]))

data_scatter <- cbind(b1,b2,b3,b4)



#零-均值规范化
data_zscore <- scale(data)


#小数定标规范化
i1 <- ceiling(log(max(abs(data[,1])),10))
c1 <- data[,1]/10^i1

i2 <- ceiling(log(max(abs(data[,2])),10))
c2 <- data[,2]/10^i2

i3 <- ceiling(log(max(abs(data[,3])),10))
c3 <- data[,3]/10^i3

i4 <- ceiling(log(max(abs(data[,4])),10))
c4 <- data[,4]/10^i4

data_dot <- cbind(c1,c2,c3,c4)


#打印结果
options(digits = 4)
data;data_scatter;data_zscore;data_dot


###################################
#4-3 数据离散华

data <- read.csv('./data/discretization_data.csv',header=T)


#等宽离散化
v1 <- ceiling(data[,1]*10)

#等频离散化
names(data) <- 'f' #
attach(data)
seq(0,length(f),length(f)/6)
v <- sort(f)
v2 <- rep(0,930)
for(i in 1:930) v2[i] <- ifelse(f[i] <= v[155],1,
                                ifelse(f[i] <= v[310],2,
                                       ifelse(f[i] <= v[465],3,
                                              ifelse(f[i] <= v[620],4,
                                                     ifelse(f[i] <= v[775],5,6)))))
detach(data)


#聚类离散化
result <- kmeans(data,6)
v3 <- result$cluster


#图示结果
par(mfrow = c(2,2))
plot(data[,1],v1, xlab = '肝气郁结证型系数')
plot(data[,1],v2, xlab = '肝气郁结证型系数')
plot(data[,1],v3, xlab = '肝气郁结证型系数')

dotchart(data[,1],xlab = '肝气郁结证型系数')


###################################
#4-5 小波变换
#
N <-  1024; k <- 6
x <- ((1:N)-N/2) *2 *pi* k/N
y <- ifelse(x>0, sin(x), sin(3*x))
signal <- y+rnorm(N)/10

#
install.packages("waveslim")
library(waveslim)

#
d <- dwt(signal, n.levels=4)

#
data.frame(d$d1, d$d2, d$d3, d$d4)



##########################################
#P61 预处理主要函数

#lm 回归
x <- 1:100
y <- 12+3*x + rnorm(100,0,9)
data <- data.frame(x,y)
model <-lm(y~x ,data)
summary(model)
  
#predict 预测
x <- rnorm(4,1,7)
y <- rep(0,4)
data1 <- data.frame(x,y)
predict(model, data1)
