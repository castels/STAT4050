library(faraway)
data(death)
attach(death)

ct<-xtabs(y~penalty+defend, death)
ct.cond<-xtabs(y~penalty+defend+victim, death)

# Combined Data
freqyb = ct[2,1]/(ct[1,1]+ct[2,1])
freqyw = ct[2,2]/(ct[1,2]+ct[2,2])
freqnb = ct[1,1]/(ct[1,1]+ct[2,1])
freqnw = ct[1,2]/(ct[1,2]+ct[2,2])

penalty = c('no','yes')
black.c = c(freqnb,freqyb)
white.c = c(freqnw,freqyw)
combined<-data.frame(penalty,black.c,white.c)

# Conditional on Victim Race
# Victim White
ct.vw<-xtabs(y~penalty+defend, subset=(victim == 'w'), data = death)

freqyb.vw = ct.vw[2,1]/(ct.vw[1,1]+ct.vw[2,1])
freqyw.vw = ct.vw[2,2]/(ct.vw[1,2]+ct.vw[2,2])
freqnb.vw = ct.vw[1,1]/(ct.vw[1,1]+ct.vw[2,1])
freqnw.vw = ct.vw[1,2]/(ct.vw[1,2]+ct.vw[2,2])

black.vw = c(freqnb.vw,freqyb.vw)
white.vw = c(freqnw.vw,freqyw.vw)
victim.white<-data.frame(penalty,black.vw,white.vw)

# Victim Black
ct.vb<-xtabs(y~penalty+defend, subset=(victim == 'b'), data = death)

freqyb.vb = ct.vb[2,1]/(ct.vb[1,1]+ct.vb[2,1])
freqyw.vb = ct.vb[2,2]/(ct.vb[1,2]+ct.vb[2,2])
freqnb.vb = ct.vb[1,1]/(ct.vb[1,1]+ct.vb[2,1])
freqnw.vb = ct.vb[1,2]/(ct.vb[1,2]+ct.vb[2,2])

black.vb = c(freqnb.vb,freqyb.vb)
white.vb = c(freqnw.vb,freqyw.vb)
victim.black<-data.frame(penalty,black.vb,white.vb)

combined
victim.white
victim.black
ct.cond

summary(ct)
summary(ct.cond)

# This is Simpson's paradox because the observed trend within 
# conditional grouped data is opposite than the trend observed
# in marginal data.

# 2b) The most appropriate model would be a conditional independence
# model.

# 2c) Fit a binomial model
ww<-subset(death, defend =='w' & victim =='w')
bb<-subset(death, defend =='b' & victim =='b')
wb<-subset(death, defend =='w' & victim == 'b')
bw<-subset(death, defend =='b' & victim == 'w')
penalty.y<-c(ww$y[1], wb$y[1], bw$y[1], bb$y[1])
penalty.n<-c(ww$y[2], wb$y[2], bw$y[2], bb$y[2])
total<-(penalty.y+penalty.n)

Victim<-c(0,1,0,1)
Defend<-c(0,0,1,1)

DP<-data.frame(penalty.y,total,Victim,Defend)

binmod<-glm(cbind(penalty.y,total-penalty.y)~Defend+Victim+Victim*Defend, family = binomial, data = DP)
summary(binmod)
