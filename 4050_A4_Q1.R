library(faraway)
data(femsmoke)
attach(femsmoke)

# Fitting a binomial glm
Dead<-subset(femsmoke, dead == 'yes')
Alive<-subset(femsmoke, dead =='no')

totalDead = apply(Dead,1,function(x) sum(Dead$y[(Dead$age ==x[4] & Dead$smoker ==x[2])]))
totalAlive = apply(Alive,1,function(x) sum(Alive$y[(Alive$age ==x[4] & Alive$smoker ==x[2])]))
totals = (totalDead+totalAlive)

Smoke<-data.frame(Dead,totals)
age<-as.factor(age)
smoker<-as.factor(smoker)

# Identify the best fitting model
binmod.0<-glm(cbind(y,totals-y)~age + smoker + age*smoker, family = binomial, data = Smoke)
summary(binmod.0)
drop1(binmod.0, test = 'Chi')

binmod.1<-glm(cbind(y,totals-y)~smoker + age, family = binomial, data = Smoke)
summary(binmod.1)
drop1(binmod.1, test = 'Chi')

binmod.2<-glm(cbind(y,totals-y)~age, family = binomial, data = Smoke)
summary(binmod.2)
drop1(binmod.2, test = 'Chi')

binmod.3<-glm(cbind(y,totals-y)~smoker, family = binomial, data = Smoke)
summary(binmod.3)
drop1(binmod.3, test = 'Chi')



