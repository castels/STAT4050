food<-read.csv("/Users/SOPH/Documents/Guelph/4_Fourth_Year/W17/STAT4050/FinalProject/food.csv", header = TRUE)
profile<-read.csv("/Users/SOPH/Documents/Guelph/4_Fourth_Year/W17/STAT4050/FinalProject/profile.csv", header = TRUE)

library(faraway)
library(lme4)
library(outliers)
attach(food)
attach(profile)

# Question 1 Investigate the strength of the relationship between an individual’s age 
# and sex and their ability to accurately identify taste and olfactory sensations
# Q1a: What is the effect of age and sex on olfactory scores?
# We use a GLM because we are modelling binomial data
olfactory.mod1<-glm(cbind(sniff,12-sniff)~age, family = binomial(link = logit), data = profile)
summary(olfactory.mod1)

olfactory.mod2<-glm(cbind(sniff,12-sniff)~age+sex, family = binomial(link = logit), data = profile)
summary(olfactory.mod2)

olfactory.mod3<-glm(cbind(sniff,12-sniff)~age + sex + age*sex, family = binomial(link = logit), data = profile)
summary(olfactory.mod3)

# Comparing Nested Models
anova(olfactory.mod1, olfactory.mod2, test = "Chi")
# Reduced model is adequate.

# Try choosing a different link function to model
olfactory.mod4<-glm(cbind(sniff,12-sniff)~age, family = binomial(link = probit), data = profile)
summary(olfactory.mod4)

olfactory.mod5<-glm(cbind(sniff,12-sniff)~age, family = binomial(link = cloglog), data = profile)
summary(olfactory.mod5)
# No significant change in AIC

# Residual Diagnostics
par(mfrow = c(1,3))
plot(olfactory.mod1)
plot(residuals(olfactory.mod1, type ="deviance")~predict(olfactory.mod1, type="link"), pch=1, main="Deviance Residuals", xlab="eta", ylab=" Deviance Residuals")
halfnorm(residuals(olfactory.mod1, type="pearson"), pch=1, main="Pearson Residuals")
halfnorm(cooks.distance(olfactory.mod1), pch=1, main="Cook's Deviance")
# Plots indicate some outliers

# Model Overdispersion
disp.param<-sum(residuals(olfactory.mod1, type="pearson")^2)/(nrow(profile)-2)
summary(olfactory.mod1, dispersion = disp.param)
# No change, since deviance residuals have relatively constant variance
# Standard errors actually increased following the addition of the dispersion parameter

# Look at any strong outliers, lets remove them (may improve model fit)
# Any Cooks D > 4/n = 4/129 = 0.03100775 are considered large (Wikipedia)
summary(influence.measures(olfactory.mod1))
# No real outliers, but lets try removing them

# Removing outliers
row42<-profile[42,]
row51<-profile[51,]
outliers<-data.frame(rbind(row42,row51))
profile_new = profile[!(profile$ID %in% outliers$ID),]

# Model using new data set
olfactory.mod6<-glm(cbind(sniff,12-sniff)~age, family = binomial(link = logit), data = profile_new)
summary(olfactory.mod6)
#AIC has increased but Null Deviance is better. Problem: Now, nothing is significant. 

# Try modelling using a quadratic term, perhaps the rate of deterioration increases with age.
age2<-age^2
olfactory.modq<-glm(cbind(sniff,12-sniff)~age + age2, family = binomial(link= logit), data = profile)

par(mfrow = c(1,3))
plot(residuals(olfactory.modq, type ="deviance")~predict(olfactory.modq, type="link"), pch=1, main="Deviance Residuals", xlab="eta", ylab=" Deviance Residuals")
halfnorm(residuals(olfactory.modq, type="pearson"), pch=1, main="Pearson Residuals")
halfnorm(cooks.distance(olfactory.modq), pch=1, main="Cook's Deviance")

# Final call:
# Use the quadratic model with just age on sniff score.  No transformation, no overdispersion, no
# change of link function, no removal of outliers.

p<-ilogit(predict(olfactory.modq))
print(mean(p))

# Q1b: What is the effect of age and sex on taste scores?

gust.mod1<-glm(cbind(taste, 16-taste)~age, family=binomial(link = logit), data = profile)
summary(gust.mod1)
gust.mod2<-glm(cbind(taste, 16-taste)~age + sex, family=binomial(link = logit), data = profile)
summary(gust.mod2)

# Comparing Nested Models
anova(gust.mod1, gust.mod2, test = "Chi")
# Reduced model is adequate/ sex not significant

# Residual diagnostics
par(mfrow = c(1,3))
plot(gust.mod1)
plot(residuals(gust.mod1, type ="deviance")~predict(gust.mod1, type="link"), pch=1, main="Deviance Residuals", xlab="eta", ylab=" Deviance Residuals")
halfnorm(residuals(gust.mod1, type="pearson"), pch=1, main="Pearson Residuals")
halfnorm(cooks.distance(gust.mod1), pch=1, main="Cook's Deviance")

# No strong outliers and variance is pretty constant
summary(influence.measures(gust.mod2))
# we are good!

# let's try to make those residuals smaller... Model Overdispersion
disp.param2<-sum(residuals(gust.mod1, type="pearson")^2)/(nrow(profile)-2)
summary(gust.mod1, dispersion = disp.param2)
# No effect. Variance in deviance residuals is constant

# Try modelling using a quadratic term, perhaps the rate of deterioration increases with age.
gust.modq<-glm(cbind(taste,16-taste)~age + age2, family = binomial(link = logit), data = profile)
summary(gust.modq)
# Model fit is worse - AIC increases, nothing is significant.

# P(Success|Gustatory)
s = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*x)
print(mean(s))

# Plots Superimposed with their means
# Plot of Olfactory/Gustatory Identification with Age
x=seq(18,100,0.1)
x2<-x^2
p = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*x + olfactory.modq$coef[3]*x2)
print(mean(p))

par(mfrow = c(1,1))
w = 0
v = 0
plot(w,v, xlab = "Age", ylab = "Sensory Accuracy", xlim = c(18,100), ylim = c(0.3,1.0), col = 'tomato')
lines(x,p, col = 'tomato')
lines(x,s, col = 'dodgerblue1')
abline(h = mean(ilogit(predict(olfactory.modq))), lty = 2, col = 'tomato')
abline(h = mean(s), lty = 2, col = 'dodgerblue1')


# Taste identification is, on average, less than olfactory identification
mean(p)/mean(s)

# Determining mean gustatory accuracy over a 20 year period
x1<-seq(20,40,0.1)
x12<-(x1)*(x1)
s1 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*x1)
p1 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*x1 + olfactory.modq$coef[3]*x12)
mean(s1)
mean(p1)

x2<-seq(40,60,0.1)
x22<-x2*x2
s2 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*x2)
p2 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*x2 + olfactory.modq$coef[3]*x22)
mean(p2)
mean(s2)

x3<-seq(60,80,0.1)
x32<-x3*x3
s3 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*x3)
p3 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*x3 + olfactory.modq$coef[3]*x32)
mean(p3)
mean(s3)

x4<-seq(80,100,0.1)
x42<-x4*x4
s4 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*x4)
p4 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*x4 + olfactory.modq$coef[3]*x42)
mean(p4)
mean(s4)
# Determining change in olfactory accuracy over 20 year periods
p20 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*20 + olfactory.modq$coef[3]*400)
p40 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*40 + olfactory.modq$coef[3]*1600)
p60 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*60 + olfactory.modq$coef[3]*3600)
p80 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*80 + olfactory.modq$coef[3]*6400)
p100 = ilogit(olfactory.modq$coef[1] + olfactory.modq$coef[2]*100 + olfactory.modq$coef[3]*10000)

# Determining change in gustatory accuracy over 20 year periods
s20 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*20)
s40 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*40)
s60 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*60)
s80 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*80)
s100 = ilogit(gust.mod1$coef[1] + gust.mod1$coef[2]*100)


# On average, folks are better at Olfactory id. than Taste id. by a factor of 1.22
# Why is this? Perhaps olfaction is such a large part of taste, its sense is stronger

#####################################################################################


Sweet<-glm(cbind(sweet,4-sweet)~age, family=binomial, data = profile)
summary(Sweet)
Salty<-glm(cbind(salty,4-salty)~age, family=binomial, data = profile)
summary(Salty)
Sour<-glm(cbind(sour,4-sour)~age, family=binomial, data = profile)
summary(Sour)
Bitter<-glm(cbind(bitter,4-bitter)~age, family=binomial, data = profile)
summary(Bitter)

Sweet.s<-glm(cbind(sweet,4-sweet)~sex, family=binomial, data = profile)
summary(Sweet.s)
Salty.s<-glm(cbind(salty,4-salty)~sex, family=binomial, data = profile)
summary(Salty.s)
Sour.s<-glm(cbind(sour,4-sour)~sex, family=binomial, data = profile)
summary(Sour.s)
Bitter.s<-glm(cbind(bitter,4-bitter)~sex, family=binomial, data = profile)
summary(Bitter.s)

tastes<-na.omit(data.frame(food$sweet,food$salty,food$sour,food$bitter))
Tastes<-xtabs(tastes)

#############################################
# Question 2
#Question 2: Determine whether or not an individual’s taste/olfactory scores can be 
# used to predict their liking of tomato soup and oatmeal.
# Step one: Model flavours as treatment fixed effects, subject ID as a random effect, see if there is any significant difference
# Determine whether or not an individual’s taste/olfactory scores can be used to predict the CATAs
# (Choose All That Apply) that they will select for oatmeal and tomato soup samples and their 
# subsequent liking/influence on purchasing

bitom.mm<-glmer(cbind(tliking, 9-tliking)~sweet+salty+sour+bitter+as.factor(tfort)+(1|ID), family=binomial)
bioat.mm<-glmer(cbind(oliking, 9-oliking)~sweet+salty+sour+bitter+as.factor(ofort)+(1|ID), family=binomial)

#Question 3
# Establish any specific texture, flavour, vitamin fortification level, and appearance qualities 
# that have the most influence on a consumer’s oatmeal and tomato soup preference and their 
# inclination to purchase the product, accounting for any confounding variables (eg. purchasing frequency).

# Subset each level of fortification
as.factor(tfort)
tf.0food<-subset(food, tfort == '0')
tf.0.5food<-subset(food, tfort == '0.5')
tf.1food<-subset(food,tfort == '1')

# See if any attributes within fortification level have an effect on liking
tf.0<-glm(cbind(tliking,9-tliking) ~ tappearance + ttexture + tflavour + tsour + not.sour + tsweet + 
            very.sweet + bland + strong + weak + toff + spicy + fruity, 
          family = binomial(link = logit), data = tf.0food)

# limitation of the model: flavour is so descriptive, so obviously it would be very signifiant.
# as well, people have difference preferences (eg some people may associate chunkiness to high liking, others not)
# so we can't really make conclusions on this in general.

# Investigating good qualities and fortification level (remove flavour as a variable): fitting mixed models
tliking<-glm(cbind(tliking,9-tliking) ~ tappearance + ttexture + tsour + not.sour + tsweet + 
          very.sweet + bland + strong + weak + toff + spicy + fruity,
        family = binomial(link = logit), data = food)
tliking.m<-glmer(cbind(tliking,9-tliking) ~ tappearance + ttexture + tsour + not.sour + tsweet + 
               very.sweet + bland + strong + weak + toff + spicy + fruity + (1|ID),
             family = binomial(link = logit), data = food)

oliking<-glm(cbind(oliking,9-oliking) ~ oappearance + otexture + 
               moist + sticky + grainy + thick + off + oat + yellow + bland + white + creamy,
             family = binomial(link = logit), data = food)
oliking.m<-glmer(cbind(oliking,9-oliking) ~ oappearance + otexture + 
               moist + sticky + grainy + thick + off + oat + yellow + bland + white + creamy + (1|ID),
             family = binomial(link = logit), data = food)
# estimates of variance for random effect (ID) not signifciantly different from zero, indicating that variability
# between subjects is not significant. We only have a significant fixed effect

# Checking to see if age with fortification level has an effect on liking
# Single variable
tliking.f<-glm(cbind(tliking,9-tliking) ~ as.factor(tfort), family = binomial, data = food)
# Full model with interaction
tliking.fa<-glm(cbind(tliking,9-tliking) ~ age + as.factor(tfort) + age*as.factor(tfort), family = binomial, data = food)
# Both variables, no interaction
tliking.f.a<-glm(cbind(tliking,9-tliking) ~ age + as.factor(tfort), family = binomial(link = logit), data = food)

# Single Variable
oliking.f<-glm(cbind(oliking,9-oliking) ~ as.factor(ofort), family = binomial, data = food)
# Full model with interaction
oliking.f*a<-glm(cbind(oliking,9-oliking) ~ age + as.factor(ofort) + age*as.factor(ofort), family = binomial, data = food)
# Both variables, no interaction
oliking.f.a<-glm(cbind(tliking,9-tliking) ~ as.factor(tfort) + age, family = binomial, data = food)

# No evidence of age or the interaction between age and fortification has an effect on liking
# in either tomato soup or oatmeal

##### QUESTION 3
# TASTE SCORES IN PREDICTING CATAs
tomsourfort<-glmer(cbind(tsour, 1-tsour)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tomnsour<-glmer(cbind(tnsour, 1-tnsour)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tomvsweet<-glmer(cbind(tvsweet, 1-tvsweet)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tombland<-glmer(cbind(bland, 1-bland)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tombland<-glmer(cbind(bland, 1-bland)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial, nAGQ=4)
tomstrong<-glmer(cbind(strong, 1-strong)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tomoff<-glmer(cbind(off, 1-off)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tomspicy<-glmer(cbind(spicy, 1-spicy)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
tomfruity<-glmer(cbind(fruity, 1-fruity)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)

#OATS
                                                            
oatmoist<-glmer(cbind(moist, 1-moist)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatsticky<-glmer(cbind(sticky, 1-sticky)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatgrainy<-glmer(cbind(grainy, 1-grainy)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatthick<-glmer(cbind(thick, 1-thick)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatoff<-glmer(cbind(off, 1-off)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatoaty<-glmer(cbind(oaty, 1-oaty)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatyellow<-glmer(cbind(yellow, 1-yellow)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatbland<-glmer(cbind(bland, 1-bland)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatwhite<-glmer(cbind(white, 1-white)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)
oatcreamy<-glmer(cbind(creamy, 1-creamy)~sweet+salty+sour+bitter+as.factor(fort)+(1|ID), family=binomial)

#####Question 4
#### FORTIFICATION ACCEPTIBILITY
#Testing fortification acceptability
forttom<-glm(cbind(tliking,9-tliking) ~ as.factor(tfort), family = binomial, data = food)
summary(forttom)
fortoat<-glm(cbind(oliking,9-oliking) ~ as.factor(ofort), family = binomial, data = food)
summary(fortoat)
# OATMEAL CAN BE FORTIFIED UP TO 100%, WHILE TOMATO SOUP SHOULD ONLY BE FORTIFIED UP TO 50%. AGE WAS INSIGNIFICANT WITH NO INTERACTION



