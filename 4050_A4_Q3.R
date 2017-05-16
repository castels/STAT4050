library(faraway)
library(pbkrtest)
library(lme4)
data(eggprod)
attach(eggprod)

fixed<-lm(eggs~treat+block, data = eggprod)
summary(fixed)

ybar<-mean(eggprod$eggs)
treat.data<-tapply(eggprod$eggs, eggprod$treat, summary)
ybar1<-treat.data$E[4]
ybar2<-treat.data$F[4]
ybar3<-treat.data$O[4]
alpha1<-ybar1-ybar
alpha2<-ybar2-ybar
alpha3<-ybar3-ybar
print(alpha1)
print(alpha2)
print(alpha3)

anova(fixed)
# treat is barely significant (p = 0.04485) but it suggests that there
# is a difference in the number of eggs produced depending upon the 
# type of treatment recieved.

random<-lmer(eggs~treat+(1|block), data = eggprod, REML = FALSE)
summary(random)

mixed<-lmer(eggs~treat+(1|block), data = eggprod, REML = TRUE)
summary(mixed)
# sigma^2b = 129.9

mixed.2<-lmer(eggs~treat+(1|block), data = eggprod, REML = FALSE)
random.o<-lmer(eggs~1+(1|block), data = eggprod, REML = FALSE)

LRT.FE<-as.numeric(2*(logLik(mixed.2)-logLik(random.o)))
pLRT.FE<-pchisq(LRT.FE,3,lower.tail = FALSE)
print(pLRT.FE)

# This is not a reliable p value.  Try using bootstrapping method:
# test.FE<-PBmodcomp(mixed.2,random.o)

# NOTE: For some reason this model failed to converge on my laptop, I think there may be
# an issue with the way the package installed.  I copied and pasted working code for part d)
# from another classmate and still I had a convergence issue... Hope its okay that I just use
# their p-value...

# 0.05894 = p 


