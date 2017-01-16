###########################################
ls() 
rm(list = ls())

######################
x <- rnorm(100, mean = 5, sd=1)
mean(x)
sd(x)
summary(x)
plot(x, type='b')


demo(graphics)

######################
memory.limit

.packages(all.available = TRUE)


######################
#数据集
dim(data()$results)
ddddd <- data()$results

######################

dist
plot
methods(plot)
plot.ecdf


######################
head(CO2,10)
CO2
tail(CO2, 10)

View(CO2)


data <- data.frame()
edit(data)


array

table

ct <- data.frame(Vote.for.X = factor(c("Yes", "Yes", "No", "Not Sure", "No"), levels = c("Yes", "No", "Not Sure")), 
                 Vote.for.X.Last.Time =  factor(c("Yes", "No", "No", "Yes", "No"), levels = c("Yes", "No"))  )
ctnames <- colnames(ct) 
colnames(ct)  <- c('x1', 'x2')

cttab <-table(ct) 

attributes(cttab)

summary(cttab) 
str(cttab) 
mode(cttab)

addmargins(cttab) 
dimnames(cttab)

subtable

g <- "My First List"
h <- c(25, 26, 18, 39, 40, 44)
j <- matrix(1:10, nrow = 5)
k <- c("one", "two", "three")
mylist <- list(title = g, ages = h, j, k)

str(mylist)
summary(mylist)
attributes(mylist)


