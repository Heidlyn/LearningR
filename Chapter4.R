getwd()
setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter4\\示例程序")

#load("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战/图书配套数据、代码\\chapter4\\示例程序\\.RData")


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
model <- lm(sales~date,date=inputfile1)  ####报错


#ls() 
#rm(list = ls())



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





