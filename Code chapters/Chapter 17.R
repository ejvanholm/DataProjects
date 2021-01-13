
par(mfrow=c(1,3))
set.seed(100)


x <- 1:100
y <- 1:100
y2 <- y^2
y3 <- log(y)

plot(x,jitter(y,25),col='peru', pch=16, main='Linear: y~x', xlab="", ylab="")
lines(x,y, col="steelblue", lwd=2)
plot(x,jitter(y2,500),col='peru', pch=16,main='Squared: y^2~x',lwd=3, xlab="", ylab="")
abline(lm(y2~x), lwd=1, lty=3, col="gray70")

lines(x,y2, col="steelblue", lwd=2)

plot(x,jitter(y3,50),col='peru', pch=16,main='Logged: log(y)~x',lwd=3, xlab="", ylab="")
abline(lm(y3~x), lwd=1, lty=3, col="gray70")

lines(x,y3, col="steelblue", lwd=2)



library(dplyr)
data("Boston", package = "MASS")
plot(medv ~ lstat, data = Boston, xlab="% in Poverty", ylab="Median Home Prices", main="Home Prices and Poverty around Boston", pch=16, col="peru")


library(dplyr)
data("Boston", package = "MASS")
plot(medv ~ lstat, data = Boston, xlab="% in Poverty", ylab="Median Home Prices", main="Home Prices and Poverty around Boston", pch=16, col="peru")
abline(lm(medv ~ lstat, data = Boston), col="steelblue", lwd=3, lty=3)


summary(lm(medv ~ lstat, data = Boston))


Boston$lstat_sq <- Boston$lstat^2
Boston$lstat_3 <- Boston$lstat^3

fit <- lm(medv ~ lstat + lstat_sq, data = Boston)
Boston$pred = predict(fit, data = Boston)

plot(medv ~ lstat, data = Boston, xlab="% in Poverty", ylab="Median Home Prices", main="Home Prices and Poverty around Boston", pch=16, col="peru")
Boston <- arrange(Boston, lstat)
with(Boston, lines(x = lstat, y = pred, col="steelblue", lwd=2))

summary(lm(medv ~ lstat+lstat_sq, data = Boston))



par(mfrow=c(1,2))
x <- 1:100
y<- -49:50
y2 <- y^2
y3 <- y^3
y4 <- y^4

plot(x,y2, xaxt='n', yaxt="n", bty="l", pch=16, cex=.5, type="l", lwd=2, main="Concave Curve", xlab="", ylab="")


plot(x,-y2, xaxt='n', yaxt="n", bty="l", pch=16, cex=.5, type="l", lwd=2, main="Convex Curve", xlab="", ylab="")


Boston$lstat_sq <- Boston$lstat^2
fit1 <- lm(medv ~ lstat, data = Boston)
fit2 <- lm(medv ~ lstat + lstat_sq, data = Boston)
Boston$pred = predict(fit2, data = Boston)

plot(medv ~ lstat, data = Boston, xlab="% in Poverty", ylab="Median Home Prices", main="Home Prices and Poverty around Boston", pch=16, col="peru")
Boston <- arrange(Boston, lstat)
with(Boston, lines(x = lstat, y = pred, col="steelblue", lwd=2))

Boston$lstat_sq <- Boston$lstat^2
Boston$lstat_cube <- Boston$lstat^3
fit1 <- lm(medv ~ lstat, data = Boston)
fit2 <- lm(medv ~ lstat + lstat_sq, data = Boston)
fit3 <- lm(medv ~ lstat + lstat_sq+lstat_cube, data = Boston)
Boston$pred = predict(fit3, data = Boston)

plot(medv ~ lstat, data = Boston, xlab="% in Poverty", ylab="Median Home Prices", main="Home Prices and Poverty around Boston", pch=16, col="peru")
Boston <- arrange(Boston, lstat)
with(Boston, lines(x = lstat, y = pred, col="steelblue", lwd=2))

summary(lm(medv ~ lstat+lstat_sq+lstat_cube, data = Boston))

### Logged variables

par(mfrow=c(2,2))
plot(Boston$lstat, Boston$medv, main="Neither Logged", pch=16, cex=.75, col="peru" , bty="l", xlab="", ylab="")
abline(lm(Boston$medv~Boston$lstat), col="steelblue", lty=3, lwd=3)
plot(Boston$lstat, log(Boston$medv), main="Log of Median Home Values", pch=16, cex=.75, col="peru", bty="l", xlab="", ylab="")
abline(lm(log(Boston$medv)~Boston$lstat), col="steelblue", lty=3, lwd=3)

plot(log(Boston$lstat), Boston$medv, main="Log of Poverty", pch=16, cex=.75, col="peru", bty="l", xlab="", ylab="")
abline(lm(Boston$medv~log(Boston$lstat)), col="steelblue", lty=3, lwd=3)

plot(log(Boston$lstat), log(Boston$medv), main="Both logged", pch=16, cex=.75, col="peru", bty="l", xlab="", ylab="")
abline(lm(log(Boston$medv)~log(Boston$lstat)), col="steelblue", lty=3, lwd=3)

summary(lm(log(medv)~log(lstat), data=Boston))

summary(lm(log(medv)~lstat, data=Boston))

#### Interpreting Linear-Log


summary(lm(medv~log(lstat), data=Boston))


### Interactions


PSID1982 <- read.csv("https://raw.githubusercontent.com/ejvanholm/DataProjects/master/PSID1982.csv")
summary(lm(wage~gender+ethnicity, data=PSID1982))


summary(lm(wage~gender*ethnicity, data=PSID1982))



with(PSID1982, tapply(wage, list(ethnicity=ethnicity,gender=gender), mean) )


