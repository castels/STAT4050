#Assignment 2
#Question 2
#Reading in the data set
embryo<-read.csv("/Users/SOPH/Desktop/embryo.csv")
attach(embryo)
library(faraway)

#########################################################################

#Variable definitions 
lcentrifuge = log(centrifuge)
success<-y/n

#Main effects model
model.1<-glm(cbind(y,n-y)~lcentrifuge+storage, family = binomial, data = embryo)
summary(model.1)

#Interaction model
model.2<-glm(cbind(y,n-y)~lcentrifuge+storage+lcentrifuge*storage, family="binomial", data = embryo)
summary(model.2)

#Untransformed model
model.3<-glm(cbind(y,n-y)~centrifuge+storage, family="binomial", data = embryo)
summary(model.3)

#Simple model
model.4<-glm(cbind(y,n-y)~lcentrifuge, family="binomial", data = embryo)
summary(model.4)


#Plotting residuals
library(faraway)
par(mfrow = c(1,3))
plot(residuals(model.1, type ="deviance")~predict(model.1, type="link"), pch=20, main="Deviance Residuals", xlab="eta", ylab=" Deviance Residuals")
halfnorm(residuals(model.1, type="pearson"), main="Pearson Residuals")
halfnorm(cooks.distance(model.1), main="Cook's Deviance")

#Plotting observed vs predicted
T = subset(embryo, storage == 't') 
Treatment = cbind(T$y/T$n, T)
C = subset(embryo, storage == 'c') 
Control = cbind(C$y/C$n, C)

### X2 has a value of 1 if treatment, 0 if control
#Making the regression line
x<-seq(0,8,0.1)
lTrt<-ilogit(model.1$coef[1] + model.1$coef[3] + model.1$coef[2]*x)
lCtrl<-ilogit(model.1$coef[1] + model.1$coef[2]*x)

par(mfrow = c(1,1))
plot(x, lTrt, type="l", xlim = c(0,8), ylim = c(0,1), xlab="log(centrifuge)", ylab="P(anther)", pch=20, col="blue")
par(new=TRUE)
plot(x, lCtrl, type="l", xlim = c(0,8), ylim = c(0,1), xlab="log(centrifuge)", ylab="P(anther)", pch=10, col="red")

#Plotting the points
pTrt<-ilogit(model.1$coef[1] + model.1$coef[3] + model.1$coef[2]*lcentrifuge)
pCtrl<-ilogit(model.1$coef[1] + model.1$coef[2]*lcentrifuge)

points(log(Treatment$centrifuge), Treatment[,1], pch=5)
points(log(Control$centrifuge), Control[,1], pch=10)

#Comparing main effects with interaction
anova(model.1,model.2, test = "Chi")

#Comparing main effects with untransformed
anova(model.1,model.3, test = "Chi")




