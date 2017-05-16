library(faraway)
data(esdcomp)
head(esdcomp)
attach(esdcomp)

#3a)
par(mfrow = c(2,2))
plot(residency,rate, xlab = "Residency", ylab = "Complaint Rate", main="Rate v.s. Residency")
plot(gender,rate, xlab = "Gender", ylab = "Complaint Rate", main="Rate v.s. Gender")
plot(revenue,rate, xlab = "Revenue($)", ylab = "Complaint Rate", main="Rate v.s. Revenue")
plot(hours,rate, xlab = "Hours", ylab = "Complaint Rate", main="Rate v.s. Hours Worked")

#3b)
binmod<-glm(cbind(complaints, visits-complaints)~residency + gender + revenue + hours, family = binomial, data = esdcomp)
plot(residuals(binmod), main = "Residuals")
halfnorm(residuals(binmod, type="pearson"), main="Pearson Residuals")
halfnorm(cooks.distance(binmod), main="Cook's Deviance")
deviance(binmod)

#3c)
poismod<-glm(complaints~residency + gender + revenue + hours + offset(log(visits)), family = poisson, data = esdcomp)
plot(residuals(poismod), main = "Residuals")
halfnorm(residuals(poismod, type="pearson"), main="Pearson Residuals")
halfnorm(cooks.distance(poismod), main="Cook's Deviance")
deviance(poismod)

#3d)
summary(poismod)

#3e)
poismod2<-glm(complaints~residency + gender + revenue + visits + offset(log(hours)), family = poisson, data = esdcomp)
summary(poismod2)

#3f)
anova(poismod,poismod2)



