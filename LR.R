################################################################################
## Packages ####
################################################################################
library(ISLR)
library(MASS)
library(dplyr)
library(cowplot) # plot_grid
library(pheatmap)
library(reshape)
require(ggplot2)
require(knitr) # kable
require(glmnet) # ???
library(randomForest)
set.seed(123)
setwd("C:\Users\OPEN\Documents\R_workingdirectory\ISLR")
options(digits = 4)

################################################################################
## Database Boston ####
################################################################################
fix(Boston)
names(Boston)
View(Boston)


cor(Boston[-14])
fit1 = lm(medv~lstat, Boston)
summary(fit1)
names(fit1)
confint(fit1)

ggplot(fit1, aes(x=fit1$fitted.values, y=fit1$residuals))+
  geom_point(shape=1) +
  geom_smooth(method="lm", col="red", se=FALSE)+
  geom_quantile(quantiles=c(0.10, 0.90),lambda=0.1)

ggplot(fit1, aes(x=fit1$fitted.values, y=fit1$residuals))+
  geom_point(shape=1) +
  geom_smooth(method=lm, col="red")

pairs(Boston)

fit2 = lm(medv~lstat+rm, Boston)
summary(fit2)

fit3 = lm(medv~lstat+rm+crim, Boston)
summary(fit3)
ggplot(fit3, aes(x=fit3$fitted.values, y=fit3$residuals))+
  geom_point(shape=1) +
  geom_smooth(method="lm", col="red", se=FALSE)+
  geom_quantile(quantiles=c(0.10, 0.90),lambda=0.1)

ggplot(fit3, aes(x=fit3$fitted.values, y=fit3$residuals))+
  geom_point(shape=1) +
  geom_smooth(method="lm", col="red")
 

fit4 = lm(medv~lstat+rm+crim+black, Boston)
summary(fit4)


naggplot(fit4, aes(x=fit4$fitted.values, y=fit4$residuals))+
  geom_point(shape=1) +
  geom_smooth(method="lm", col="red", se=FALSE)+
  geom_quantile(quantiles=c(0.10, 0.90),lambda=0.1)

ggplot(fit4, aes(x=fit4$fitted.values, y=fit4$residuals))+
  geom_point(shape=1) +
  geom_smooth()

anova(fit4)
confint(fit4, level=0.95)
vcov(fit4)

anova(fit1, fit2, fit3,  fit4, fit6)
step <- stepAIC(fit4, direction="both")

fit5 = lm(medv~lstat+rm+crim+black+age, Boston)
summary(fit5)

step2 <- stepAIC(fit6, direction="both")

fit6 = lm(medv~lstat+rm+crim+black+chas+ptratio, Boston)
summary(fit6)

library(leaps)
leaps<-regsubsets(medv~lstat+rm+crim+black+chas, Boston, nbest=10)

fit7 = lm(medv~lstat+rm+crim+black+chas+ptratio+tax+dis+age+nox+zn+indus+rad, Boston)
step3 <- stepAIC(fit.all, direction="both")

fit8 = lm(medv ~ lstat + rm + crim + black + chas + ptratio + dis + nox + zn, Boston)
summary(fit8)

fit9 = lm(medv ~lstat + rm + crim + black + chas + ptratio + tax + dis + nox + zn + rad, Boston)
summary(fit9)


ggplot(fit6, aes(x=fit6$fitted.values, y=fit6$residuals))+
  geom_point(shape=1) +
  geom_smooth()

fit.all = lm(medv~., Boston)
summary(fit.all)
summary(fit.all)$sigma #RSE
summary(fit.all)$fstatistic
summary(fit.all)$r.sq # R^2

library(car)
vif(fit.all)
fit10 <- lm(medv~.-age, Boston)
#or
fit10 <- update(fit10, ~.-age)

fit11 <- lm(medv~lstat*age, Boston)
summary(fit11)

fit12 <- lm(medv~lstat+I(lstat^2), Boston)
summary(fit12)
ggplot(fit12, aes(x=fit12$fitted.values, y=fit12$residuals))+
  geom_point(shape=1) +
  geom_smooth()+
  geom_quantile(quantiles=c(0.1, 0.9),lambda=0.5, stat = "quantile", colour="blue")



par(mfrow =c(2,2))
plot(fit12)
dev.off()
plot(predict (fit1), residuals (fit1))
plot(predict (fit1), rstudent (fit1))
plot(hatvalues (fit1))
which.max (hatvalues (fit1))

fit13=lm(medv~poly(lstat ,5), Boston)


attach (Carseats )
names(Carseats)
head(Carseats$ShelveLoc)
contrasts(Carseats$ShelveLoc)
fit14 <- lm(Sales~Income+ Population+ShelveLoc, Carseats)
summary(fit14)


rf.bh <- randomForest(medv~., Boston)
rf.bh
