
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



