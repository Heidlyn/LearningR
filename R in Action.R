
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


