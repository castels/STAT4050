library(faraway)
data(orings)
pseudo<-glm(cbind(damage, 6-damage)~temp, family = binomial, data = orings)
pR2 = 1-pseudo$deviance/pseudo$null.deviance
