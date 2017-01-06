
getwd()

setwd("E:\\R\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter8\\chapter8\\示例程序")

setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter8\\chapter8\\示例程序")

ls() 
rm(list = ls())

###################################

#8-1 数据聚类离散化 P168

## 读取数据
datafile <- read.csv('./data/data.csv',header = T)

##参数初始化
type <- 4
index <- 8
typelable <- c('A', 'B', 'C', 'D', 'E', 'F')
set.seed(1234)
cols <- ncol(datafile[, 1:6])
rows <- nrow(datafile[, 1:6])
disdata <- matrix(NA, rows, cols+1)

##聚类离散化
for(i in 1:cols){
  cl <- kmeans(datafile[, i], type, nstart = 20);
  disdata[,i] <- paste(typelable[i], cl$cluster)
}


disdata[, cols+1] <- datafile[, index]
disdata[, cols+1] <- paste('H', disdata[, cols+1], sep = '')


##导出数据
colnames(disdata) <- c('肝气郁证型系数', '热毒蕴结证型系数', '冲任失调证型系数', 
                       '气血两虚证型系数', '脾胃虚弱证型系数', '肝肾阴虚证型系数', 'TNN分期')
write.csv(disdata, file = './tmp/processedfile.csv', quote = F, row.names = F)



###################################

#8-2 Apriori关联规则 P170

install.packages('arules')
library(arules)

##读入数据
a <- read.csv('./tmp/processedfile.csv', header = T)
trans <- as(a, 'transactions')
inspect(trans[1:5])

#调用Apriori算法
rules <- apriori(trans, parameter = list(support=0.06, confidence=0.75))

rules
inspect(rules)

###################################


