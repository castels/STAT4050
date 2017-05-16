### STAT 4050 
#### Assignment 3
##### Q2

library(faraway)
data(salmonella)
head(salmonella)
attach(salmonella)

#2a)
plot(dose, colonies, main = "Salmonella Colonies per Dose Quinoline", ylab = "# of Colonies", xlab = "Dosage")

#Isolating each dose              
D0<-subset(salmonella, dose =="0")
D10<-subset(salmonella, dose =="10")
D33<-subset(salmonella, dose =="33")
D100<-subset(salmonella, dose =="100")
D333<-subset(salmonella, dose =="333")
D1000<-subset(salmonella, dose =="1000")

#Calculating mean and variance within each group
E0<-mean(D0$colonies)
E10<-mean(D10$colonies)
E33<-mean(D33$colonies)
E100<-mean(D100$colonies)
E333<-mean(D333$colonies)
E1000<-mean(D1000$colonies)
means<-c(E0,E10,E33,E100,E333,E1000)

V0<-var(D0$colonies)
V10<-var(D10$colonies)
V33<-var(D33$colonies)
V100<-var(D100$colonies)
V333<-var(D333$colonies)
V1000<-var(D1000$colonies)
vars<-c(V0,V10,V33,V100,V333,V1000)

#2b)
plot(means,vars, main="Mean # of Colonies vs. Variance within Dosage", 
     xlab = "Mean", ylab = "Variance")

#2c)
dosef<-as.factor(dose)
mod1<-glm(colonies~dosef)
summary(mod1)
plot(dosef,fitted(mod1), xlab= "Dosage", ylab = "Predicted # of Colonies", main = "Model 1")

#2d)
plot(residuals(mod1), main = "Residuals")
halfnorm(residuals(mod1, type="pearson"), main="Pearson Residuals")
halfnorm(cooks.distance(mod1), main="Cook's Deviance")
qqnorm(residuals(mod1))
qqline(residuals(mod1))

#2e)
disp.param<-sum(residuals(mod1, type="pearson")^2)/12
dose<-factor(dose)
mod2<-glm(colonies~dose, data = salmonella, family = "poisson")
summary(mod2, dispersion = disp.param)
par(mfrow = c(2,2))
plot(residuals(mod2), main = "Residuals")
halfnorm(residuals(mod2, type="pearson"), main="Pearson Residuals")
halfnorm(cooks.distance(mod2), main="Cook's Deviance")

#2f)
eta500 = predict(mod2,newdata = data.frame(dose = 500))
exp(eta500)

z = 1.96
X0 = c(1,500)
var = vcov(mod2) 
SE = sqrt(X0%*%(var%*%X0))

CIlower = exp(eta500-SE*z)
CIupper = exp(eta500+SE*z)

print(paste('(',CIlower,',',CIupper,")"))



