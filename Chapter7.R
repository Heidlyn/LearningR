
getwd()

setwd("E:\\R\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter7\\chapter7\\示例程序")

setwd("G:\\华工研究生\\学习教程\\R语言数据分析与挖掘实战\\图书配套数据、代码\\chapter7\\chapter7\\示例程序")

ls() 
rm(list = ls())


###################################

#7-1 数据探索 P150

datafile <- read.csv('./data/air_data.csv',header = T)

col <- c(15:18, 20:29)

summary(datafile[, col])


###################################

#7-2 数据清洗 P150

#丢弃票价为空的记录
delet_na <- datafile[-which(is.na(datafile$SUM_YR_1) | is.na(datafile$SUM_YR_2)), ]

#丢弃票价为0、折扣不为0、里程大于0 的记录
index <- ( (delet_na$SUM_YR_1 == 0 & delet_na$SUM_YR_2==0)
           * (delet_na$avg_discount != 0)
           * (delet_na$SEG_KM_SUM>0))

deletdata <- delet_na[-which(index ==1),]

cleanedfile <- deletdata

##自己增加代码
#LRFMC_src_data <- data.frame(cleanedfile$LOAD_TIME, cleanedfile$FFP_DATE,  cleanedfile$LAST_TO_END, cleanedfile$FLIGHT_COUNT, cleanedfile$SEG_KM_SUM, cleanedfile$avg_discount) 
LRFMC_src_data <- cleanedfile[c('LOAD_TIME','FFP_DATE', 'LAST_TO_END', 'FLIGHT_COUNT', 'SEG_KM_SUM', 'avg_discount')]

colna <- c('LOAD_TIME','FFP_DATE', 'LAST_TO_END', 'FLIGHT_COUNT', 'SEG_KM_SUM', 'AVG_DISCOUNT')
colnames(LRFMC_src_data) <- colna

#L <- as.numeric(LRFMC_src_data$LOAD_TIME) - as.numeric(LRFMC_src_data$FFP_DATE)
L <- difftime(LRFMC_src_data$LOAD_TIME, LRFMC_src_data$FFP_DATE,units="days")  #只能计算日期差，还可以是“secs”, “mins”, “hours”, “days”
R <- LRFMC_src_data$LAST_TO_END
FF <- LRFMC_src_data$FLIGHT_COUNT
M <- LRFMC_src_data$SEG_KM_SUM
C <- LRFMC_src_data$AVG_DISCOUNT

LRFMC_data <- data.frame(L, R, FF, M, C)

summ <- summary(LRFMC_data$R)

###################################

#7-3 标准化 P152


datafile <- read.csv('./data/zscoredata.csv',header = T)

#数据标准化
zscoredfile <- scale(datafile)
colnames(zscoredfile) = c('ZL','ZR','ZF','ZM','ZC')

#数据写入
write.csv(zscoredfile, './tmp/zscoredata.csv')


###################################
#7-4 k_Means 聚类算法

inputfile <- read.csv('./data/zscoreddata.csv', he=T)
#print(inputfile)

#聚类分析
result <- kmeans(inputfile[,2:6],5)

#结果输出
type <- result$cluster
table(type)
centervec <- result$centers

#sizes <- result$size
centers <- data.frame(result$centers)

install.packages('fmsb')
library(fmsb)
radarchart(centers)



