
###########################################
ls() 
rm(list = ls())

###########################################
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)

colors()

array()

data.frame()

list()



n <- 40 
mycolors <- rainbow(n)
pie(rep(1,n), labels = mycolors, col = mycolors)
mygrays <- gray(0:n/n)
pie(rep(1,n), labels = mygrays, col = mygrays)


dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
par(pin = c(2, 3))
par(lwd = 2, cex = 1.5)
par(cex.axis = 0.75, font.axis = 3)
plot(dose, drugA, type = "b", pch = 19, lty = 2, col = "red")
plot(dose, drugB, type = "b", pch = 23, lty = 6, col = "blue", 
     bg = "green")
par(opar)
par <- par()


plot(dose, drugA, type = "b", col = "red", lty = 1, 
     pch = 5, lwd = 2, main = "Clinical Trials for Drug A", 
     sub = "This is hypothetical data", 
     xlab = "Dosage", ylab = "Drug Response", xlim = c(0, 60), 
     ylim = c(0, 70))


# Listing 3.2 - An Example of Custom Axes

x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 8) + 0.1)

plot(x, y, type = "b", pch = 21, col = "red", yaxt = "n", 
     lty = 3, ann = FALSE)
lines(x, z, type = "b", pch = 22, col = "blue", lty = 2)
axis(2, at = x, labels = x, col.axis = "red", las = 2)
axis(4, at = z, labels = round(z, digits = 2), col.axis = "blue", 
     las = 2, cex.axis = 0.7, tck = -0.01)
mtext("y=1/x", side = 4, line = 3, cex.lab = 1, las = 2, 
      col = "blue")
title("An Example of Creative Axes", xlab = "X values", 
      ylab = "Y=X")
par(opar)



abline()


# Listing 3.3 - Comparing Drug A and Drug B response by dose
install.packages('Hmisc')

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly = TRUE)
par(lwd = 2, cex = 1.5, font.lab = 2)
plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red", 
     ylim = c(0, 60), main = "Drug A vs. Drug B", xlab = "Drug Dosage", 
     ylab = "Drug Response")
lines(dose, drugB, type = "b", pch = 17, lty = 2, 
      col = "blue")
abline(h = c(30,10), lwd = 1.5, lty = 2, col = "grey")
library(Hmisc)
minor.tick(nx = 5, ny = 5, tick.ratio = 0.5)
legend("topleft", inset = 0.05, title = "Drug Type", 
       c("A", "B"), lty = c(1, 2), pch = c(15, 17), col = c("red", 
                                                            "blue"), cex = 0.5)
par(opar)



attach(mtcars)
plot(wt, mpg, main = "Milage vs. Car Weight", xlab = "Weight", 
     ylab = "Mileage", pch = 18, col = "blue")
text(wt, mpg, row.names(mtcars), cex = 0.6, pos = 4, 
     col = "red")
mtext("test mtext", side = 4, line = -1)
detach(mtcars)


opar <- par(no.readonly = TRUE)
par(cex = 1.5)
plot(1:7, 1:7, type = "n")
text(3, 3, "Example of default text")
text(4, 4, family = "mono", "Example of mono-spaced text")
text(5, 5, family = "serif", "Example of serif text")
par(opar)

demo(plotmath)

par()




attach(mtcars)
myma <- matrix(c( 1, 2, 1,3), 2, 2, byrow = TRUE)
layout(myma)
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)



# Listing 4.2 - Creating new variables

mydata <- data.frame(x1 = c(2, 2, 6, 4), x2 = c(3, 
                                                4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2

attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)

mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2)/2)


manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", 
          "5/1/09")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, gender, age, 
                         q1, q2, q3, q4, q5, stringsAsFactors = FALSE)

leadership$age[leadership$age == 99 ] <- NA

leadership <- within(leadership, {
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young"
})



install.packages("reshape")
library(reshape)
leadership <- rename(leadership, c(q1 = "qq1"))
names(leadership)[5] <- "q1"

names(gender)

fix(leadership)


attach(leadership)
newdata <- leadership[order(-q2), ]
newdata
detach(leadership)

attach(leadership)
newdata <- leadership[order(gender, -age), ]
newdata
newdata <- leadership[order( -q2, age), ]
detach(leadership)


myvars <- names(leadership) %in% c("q3", "q4")
newdata <- leadership[!myvars]

newdata <- leadership[c(-7, -8)]

leadership$q3 <- leadership$q4 <- NULL

subset

install.packages("chron")

install.packages("sqldf")
library(tcltk)
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg", 
               row.names = TRUE)
newdf <- sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp,
    gear from mtcars where cyl in (4, 6) group by gear")

newdata <- sqldf("select * from leadership where gender ='F' ", row.names=TRUE)
sample()

install.packages('vcd')
library(vcd)
counts <- table(Arthritis$Improved)
counts

# Listing 6.1 - Simple bar plot

# simple bar plot
barplot(counts, main = "Simple Bar Plot", xlab = "Improvement", 
        ylab = "Frequency")

# horizontal bar plot
barplot(counts, main = "Horizontal Bar Plot", xlab = "Frequency", 
        ylab = "Improvement", horiz = TRUE)

plot(Arthritis$Improved, main='Simple Bar Plot', xlab = 'Improved', ylab = 'Frequncy')



# get counts for Improved by Treatment table
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

counts <- table( Arthritis$Treatment, Arthritis$Improved)

# Listing 6.2 - Stacked and groupde bar plots

# stacked barplot
barplot(counts, main = "Stacked Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), 
        legend = rownames(counts))

# grouped barplot
barplot(counts, main = "Grouped Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), 
        legend = rownames(counts), 
        beside = TRUE)




# Listing 6.3 - Mean bar plots

states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, 
                   by = list(state.region), 
                   FUN = mean)
means

means <- means[order(means$x), ]
means

barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")



# Listing 6.4 - Fitting labels in bar plots

par(mar = c(5, 8, 4, 2))
par(las = 2)
counts <- table(Arthritis$Improved)

barplot(counts, main = "Treatment Outcome", horiz = TRUE, 
        cex.names = 0.8, names.arg = c("No Improvement", 
                                       "Some Improvement", "Marked Improvement"))

library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
counts <- table(Improved, Treatment)
spine(counts, main = "Spinogram Example")
detach(Arthritis)


# Listing 6.5 - Pie charts

par(mfrow=c(2,2))
slieces <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")

pie(slieces, labels = lbls, main = "Simple Pie Chart")

pct <- round(slieces/sum(slieces)*100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slieces, labels = lbls2, col = rainbow(length(lbls2)), main = "Pie Chart with Percentage")

#install.packages("plotrix")
library(plotrix)
pie3D(slieces, labels=lbls, explode=0.05, main="3D Pie Chart" )
pie3D(slieces, labels=lbls,  main="3D Pie Chart" )

mytable <- table(state.region)
lbls3 <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls3, main = "Pie Chart from a Table \n (with sample sizes)")

fan.plot(slieces, labels = lbls, main = "Fan Plot")


hist()




# Listing 6.6 - Histograms

par(mfrow = c(2, 2))

hist(mtcars$mpg)

hist(mtcars$mpg, breaks = 12, col = "red", 
     xlab = "Miles Per Gallon", 
     main = "Colored histogram with 12 bins")

hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red", 
     xlab = "Miles Per Gallon", 
     main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = "blue", lwd = 2)

# Histogram with Superimposed Normal Curve 
# (Thanks to Peter Dalgaard)
x <- mtcars$mpg
h <- hist(x, breaks = 12, col = "red", 
          xlab = "Miles Per Gallon", 
          main = "Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
box()





par(mfrow = c(2, 1))
d <- density(mtcars$mpg)

plot(d)

d <- density(mtcars$mpg)
plot(d, main = "Kernel Density of Miles Per Gallon")
polygon(d, col = "red", border = "blue")
rug(mtcars$mpg, col = "brown")



# Listing 6.8 - Comparing kernel density plots

#install.packages("sm")

par(lwd = 2)
library(sm)
attach(mtcars)

cyl.f <- factor(cyl, levels = c(4, 6, 8), 
                labels = c("4 cylinder", "6 cylinder", "8 cylinder"))

sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")
title(main = "MPG Distribution by Car Cylinders")

colfill <- c(2: (2 + length(levels(cyl.f) ) ) )
cat("Use mouse to place legend...", "\n\n")
legend(locator(1), levels(cyl.f), fill = colfill)
detach(mtcars)
par(lwd = 1)


boxplot(mtcars$mpg, main="Box plot", ylab="Miles per Gallom")
boxplot.stats(mtcars$mpg)


boxplot(mpg ~ cyl, data = mtcars, 
        main = "Car Milage Data", 
        xlab = "Number of Cylinders", 
        ylab = "Miles Per Gallon")


boxplot(mpg ~ cyl, data = mtcars, 
        notch =TRUE,
        varwidth = TRUE,
        col = "red",
        main = "Car Milage Data", 
        xlab = "Number of Cylinders", 
        ylab = "Miles Per Gallon")


# Listing 6.9 - Box plots for two crossed factors

mtcars$cyl.f <- factor(mtcars$cyl, levels = c(4, 6, 
                                              8), labels = c("4", "6", "8"))

mtcars$am.f <- factor(mtcars$am, levels = c(0, 1), 
                      labels = c("auto", "standard"))

boxplot(mpg ~ am.f * cyl.f, data = mtcars, 
        varwidth = TRUE, col = c("gold", "darkgreen"), 
        main = "MPG Distribution by Auto Type", 
        xlab = "Auto Type")


install.packages("vioplot")



library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x1, x2, x3, 
        names = c("4 cyl", "6 cyl", "8 cyl"), 
        col = "gold")
title("Violin Plots of Miles Per Gallon")



dotchart(mtcars$mpg, labels = row.names(mtcars), 
         cex = 0.7, 
         main = "Gas Milage for Car Models", 
         xlab = "Miles Per Gallon")

# Listing 6.11 - sorted colored grouped dot chart

x <- mtcars[order(mtcars$mpg), ]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, labels = row.names(x), cex = 0.7, 
         pch = 19, groups = x$cyl, 
         gcolor = "black", color = x$color, 
         main = "Gas Milage for Car Models\ngrouped by cylinder", 
         xlab = "Miles Per Gallon")






exp(1)
log(exp(100))


# Listing 5.1 -  Calculating the mean and 
# standard deviation

x <- c(1, 2, 3, 4, 5, 6, 7, 8)
mean(x)
sd(x)
n <- length(x)
meanx <- sum(x)/n
css <- sum((x - meanx)^2)            
sdx <- sqrt(css / (n-1))
meanx
sdx

set.seed(100)
x<-rnorm(5)




# Listing 5.2 - Generating pseudo-random numbers from 
# a uniform distribution

runif(5)
runif(5)
set.seed(1234)                                                     
runif(5)
set.seed(1234)                                                      
runif(5)

# Listing 5.3 - Generating data from a multivariate 
# normal distribution

library(MASS)
options(digits=3)
set.seed(1234)

mean <- c(230.7, 146.7, 3.6)                                           
sigma <- matrix( c(15360.8, 6721.2, -47.1,                              
                   6721.2, 4700.9, -16.5,
                   -47.1,  -16.5,   0.3), nrow=3, ncol=3)

mydata <- mvrnorm(500, mean, sigma)                                     
mydata <- as.data.frame(mydata)                                         
names(mydata) <- c("y", "x1", "x2")                                       

dim(mydata)                                                             
head(mydata, n=10)   






# Listing 5.4 - Applying functions to data objects

a <- 5
sqrt(a)
b <- c(1.243, 5.654, 2.99)
round(b)
c <- matrix(runif(12), nrow=3)
c
log(c)
mean(c)

# Listing 5.5 - Applying a function to the rows 
# (columns) of a matrix

library(gdata)

mydata <- matrix(rnorm(30), nrow=6)
mydata
apply(mydata, 1, mean)     
apply(mydata, 2, mean) 
apply(mydata, 2, mean, trim=.4)   

mean(mydata[,1])



# Listing 5.6 - A solution to the learning example

options(digits=2)

Student <- c("John Davis", "Angela Williams", 
             "Bullwinkle Moose", "David Jones", 
             "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors=FALSE)

z <- scale(roster[,2:4]) 
score <- apply(z, 1, mean)                                            
roster <- cbind(roster, score)

y <- quantile(score, c(.8,.6,.4,.2))             

#roster$grade[score >= y[1]] <- "A"                                     
#roster$grade[score < y[1] & score >= y[2]] <- "B"
#roster$grade[score < y[2] & score >= y[3]] <- "C"
#roster$grade[score < y[3] & score >= y[4]] <- "D"
#roster$grade[score < y[4]] <- "F"

roster <- within(roster,{
  grade <- NA
  grade[score >= y[1]] <- "A"   
  grade[score < y[1] & score >= y[2]] <- "B"
  grade[score < y[2] & score >= y[3]] <- "C"
  grade[score < y[3] & score >= y[4]] <- "D"
  grade[score < y[4]] <- "F"
})


#name <- strsplit((roster$Student), " ")    
name <- strsplit(roster$Student, " ")   
lastname <- sapply(name, "[", 2)
firstname <- sapply(name, "[", 1)

roster <- cbind(firstname,lastname, roster[,-1])
roster <- roster[order(lastname,firstname),]

roster

sum(roster[,2])
sum(z)
apply(z, 2, sum )


# Listing 5.7 - A switch example

feelings <- c("sad", "afraid")
for (i in feelings)
  print(
    switch(i,
           happy  = "I am glad you are happy",
           afraid = "There is nothing to fear",
           sad    = "Cheer up",
           angry  = "Calm down now"
    )
  )



# Listing 5.8 - mystats(): a user-written function 
# for summary statistics

mystats <- function(x, parametric=TRUE, print=FALSE) {
  if (parametric) 
  {
    center <- mean(x); spread <- sd(x) 
  } 
  else 
  {
    center <- median(x); spread <- mad(x) 
  }
  
  if (print & parametric) 
  {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  }
  else if (print & !parametric) 
  {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  
  result <- list(center=center, spread=spread)
  return(result)
}

# trying it out
set.seed(1234)
x <- rnorm(500) 
y <- mystats(x)
y <- mystats(x, parametric=FALSE, print=TRUE)

# Another switch example
mydate <- function(type="long") {
  switch(type,
         long =  format(Sys.time(), "%A %B %d %Y"), 
         short = format(Sys.time(), "%m-%d-%y"),
         cat(type, "is not a recognized type\n"))
}
mydate("long")
mydate("short")
mydate()
mydate("medium")




# Listing 5.10 - Aggregating data

options(digits=3)
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,gear), 
                    FUN=mean, na.rm=TRUE)
aggdata



ID     <- c(1,1,2,2)
Time   <- c(1,2,1,2)
X1     <- c(5,3,6,2)
X2     <- c(6,5,1,4)
mydata <- data.frame(ID, Time, X1, X2)


install.packages('reshape')

library(reshape)
md <- melt(mydata, id=(c('ID', 'Time')))


















vars <- c("mpg", "hp", "wt")
head(mtcars[vars])

# Listing 7.1 - descriptive stats via summary

summary(mtcars[vars])

sum_min <- summary(mtcars[vars])[1]
sumchar <- strsplit(sum_min, ":")
charr   <-  sapply(sumchar, "[", 2)
num     <- as.double(charr)

summary(mtcars)


fivenum(mtcars[,1])

# Listing 7.2 - descriptive stats via sapply()

mystats <- function(x, na.omit = FALSE) {
  if (na.omit) 
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x - m)^3/s^3)/n
  kurt <- sum((x - m)^4/s^4)/n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}

sapply(mtcars[vars], mystats)

mystats(mtcars$mpg)


ttt <- t(roster)
t   <-t(ttt)



# Listing 7.3 - Descriptive statistics (Hmisc package)
#install.packages("Hmisc")
library(Hmisc)
describe(mtcars[vars])

# Listing 7.4 - Descriptive statistics (pastecs package)
#install.packages("pastecs")
library(pastecs)
stat.desc(mtcars[vars])


# Listing 7.5 - Descriptive statistics (psych package)
#install.packages("psych")
library(psych)
describe(mtcars[vars])


# Listing 7.6 - Descriptive statistics by group with aggregate()

aggregate(mtcars[vars], by = list(am = mtcars$am), mean)
aggregate(mtcars[vars], by = list(am = mtcars$am), sd)


# Listing 7.7 - Descriptive statistics by group via by()
vars <- c("mpg", "hp", "wt")
dstats <- function(x)(c(mean=mean(x), sd=sd(x)))
by(mtcars[vars], mtcars$am, mean)



# Listing 7.8 Summary statists by group (doBy package)
#install.packages("doBy")
library(doBy)
summaryBy(mpg + hp + wt ~ am, data = mtcars, FUN = mystats)



# Listing 7.9 - Summary statistics by group (psych package)

library(psych)
describeBy(mtcars[vars], mtcars$am)


# Listing 7.10 Summary statistics by group (reshape package)

library(reshape)
dstats <- function(x) (c(n = length(x), mean = mean(x), 
    sd = sd(x)))
dfm <- melt(mtcars, measure.vars = c("mpg", "hp", 
    "wt"), id.vars = c("am", "cyl"))
cast(dfm, am + cyl + variable ~ ., dstats)


# Section --7.2--

# get Arthritis data
library(vcd)
head(Arthritis)



# one way table

mytable <- with(Arthritis, table(Improved))
mytable
prop.table(mytable)
prop.table(mytable)*100


# two way table

mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable
margin.table(mytable, 1)
prop.table(mytable, 1)
margin.table(mytable, 2)
prop.table(mytable, 2)
prop.table(mytable)
addmargins(mytable)
addmargins(prop.table(mytable))
addmargins(prop.table(mytable, 1), 2)
addmargins(prop.table(mytable, 2), 1)


# Listing 7.11 - Two-way table using CrossTable
install.packages("gmodels")
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)



# Listing 7.12 - Three-way contingency table

mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)
margin.table(mytable, 1)
margin.table(mytable, 2)
margin.table(mytable, 3)
margin.table(mytable, c(1,3))
ftable(prop.table(mytable, c(1, 2)))
ftable(addmargins(prop.table(mytable, c(1, 2)), 3))

ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100


# Listing 7.13 - Chis-square test of independence

library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)


# Fisher's exact test

mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

# Chochran-Mantel-Haenszel test

mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)


# Listing 7.14 - Measures of association for a two-way table

library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)


# Listing 7.15 - converting a table into a flat file via table2flat

table2flat <- function(mytable) {
    df <- as.data.frame(mytable)
    rows <- dim(df)[1]
    cols <- dim(df)[2]
    x <- NULL
    for (i in 1:rows) {
        for (j in 1:df$Freq[i]) {
            row <- df[i, c(1:(cols - 1))]
            x <- rbind(x, row)
        }
    }
    row.names(x) <- c(1:dim(x)[1])
    return(x)
}



# Listing 7.16 - Using table2flat with published data

treatment <- rep(c("Placebo", "Treated"), 3)
improved <- rep(c("None", "Some", "Marked"), each = 2)
Freq <- c(29, 13, 7, 7, 7, 21)
mytable <- as.data.frame(cbind(treatment, improved, Freq))
mydata <- table2flat(mytable)
head(mydata)




# Listing 7.17 - Covariances and correlations

states <- state.x77[, 1:6]
cov(states)
cor(states)
cor(states, method="spearman")

x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cor(x, y)



# partial correlation of population and murder rate, controlling
# for income, illiteracy rate, and HS graduation rate
install.packages("ggm")
library(ggm)
pcor(c(1, 5, 2, 3, 6), cov(states))

# Listing 7.18 - Testing correlations for significance

cor.test(states[, 3], states[, 5], method = "pearson")

# Listing 7.19 - Correlation matrix and tests of significance via corr.test

library(psych)
corr.test(states, use = "complete")






