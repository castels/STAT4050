#Assignment 2
#Question 3
library(faraway)
library(ResourceSelection)
data(wbca)
head(wbca)
model.1<-glm(Class ~ Adhes + BNucl + Chrom + Epith + Mitos + NNucl + Thick 
             + UShap + USize , family = binomial(link="logit"), data = wbca)

#Assessing Goodness-of-Fit
test.stat<-hoslem.test(model.1$y, fitted(model.1), g=10)
test.stat
for(i in 5:15)
{
  print(hoslem.test(model.1$y, fitted(model.1), g=i)$p.value)
}

#the smaller the AIC, the better the fit
step(model.1, steps=1000)

#Obtaining fited values
model.o<-glm(Class ~ Adhes + BNucl+ Chrom + Mitos + NNucl + Thick + UShap, 
             family = binomial(link="logit"), data = wbca)

fitted<-data.frame(fitted(model.o), wbca$Class)

#0 if malignant, 1 if benign
# Adjusting: Cancer is benign if p>0.5. The function checks for the case where the observation has a fitted p>0.5 and 
# it had been previously assigned as malignant (wbca.Class=0). If so, assign a value of 1. Else, assign 0.
Benign1<-function(x)
{
  if(x[1]>0.5 && x[2] == 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

# Adjusting: Cancer is malignant if p<0.5. The function checks for the case where the observation has a fitted p<0.5 and 
# it had been previously assigned as benign (wbca.Class=1). If so, assign a value of 1. Else, assign 0.
Malignant1<-function(y)
{
  if(y[1]<0.5 && y[2] == 1)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

#Finding the sums of errors for each type
B<-subset(fitted, wbca.Class == 1)
M<-subset(fitted, wbca.Class == 0)

adj.bClass<-apply(M,1,Benign1)
adj.mClass<-apply(B,1,Malignant1)

sum(adj.bClass)
sum(adj.mClass)

# Changing the cutoff from 0.5 to 0.9
Benign2<-function(x)
{
  if(x[1]>0.9 && x[2] == 0)
  {
    return(1)
  }
  
  else
  {
    return(0)
  }
}

Malignant2<-function(y)
{
  if(y[1]<0.9 && y[2] == 1)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

#Finding the sums of errors for each type

adj.2bClass<-apply(M,1,Benign2)
adj.2mClass<-apply(B,1,Malignant2)

sum(adj.2bClass)
sum(adj.2mClass)

#Creating test and training set
tf<-c(TRUE, TRUE, FALSE)
Subset<-rep.int(tf, 227)
wbca.new<-cbind(wbca,Subset)

training = subset(wbca.new, Subset == TRUE)
test = subset(wbca.new, Subset == FALSE)

#Fitting training model
model.training<-glm(Class ~ Adhes + BNucl+ Chrom + Mitos + NNucl + Thick + UShap, 
             family = binomial(link="logit"), data = training)

#Assessing predictive performance
test.training<-data.frame(ilogit(predict(model.training, test)), test$Class)
test.training.b<-subset(test.training, test.Class == 1)
test.training.m<-subset(test.training, test.Class == 0)

test.training.b_new_1<-cbind(test.training.b, apply(test.training.b, 1, Malignant1))
test.training.m_new_1<-cbind(test.training.m, apply(test.training.m, 1, Benign1))
sum(test.training.b_new_1[,3])
sum(test.training.m_new_1[,3])

test.training.b_new_2<-cbind(test.training.b, apply(test.training.b, 1, Malignant2))
test.training.m_new_2<-cbind(test.training.m, apply(test.training.m, 1, Benign2))
sum(test.training.b_new_2[,3])
sum(test.training.m_new_2[,3])

