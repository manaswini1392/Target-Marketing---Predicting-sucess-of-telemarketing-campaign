#Appendix 2. R code for Principal Component Analysis
rm(list= ls())
library(readxl)

bank<- read_excel("E:/Class Notes/Predictive Modelling/Group Project 2/bank-additional-full.xlsx")
summary(is.na(bank))
bank$duration <- NULL
str(bank)

for(i in colnames(bank))
{
  if(class(bank[[i]]) == "character")
  {
    bank[[i]]= as.factor(bank[[i]])
  }
}

str(bank)

bank$y = ifelse(bank$y =="yes",1,0)
glm.age=glm(y~age, family = binomial, data= bank) # Fit a logistic regression for age variable
summary(glm.age) # Since age has a very small p-value, we need to keep it for further analysis.
table.job= table(bank$job, bank$y) # Since job is a categorical data, we are going to use Chi-squared Test for
#independence to test the association between job and y.
table.job
chisq.test(table.job) # Since the p-value is very small, we need to keep job for further analysis.
table.marital = table(bank$marital, bank$y) # Using Chi-squared Test for association analysis.
table.marital
chisq.test(table.marital) # Since the p-value is very small, we keep it for further analysis.
table.education = table(bank$education, bank$y) # Using Chi-squared Test for association analysis.
table.education
chisq.test(table.education) # Since the p-value is very small, we keep it for further analysis.
table.default = table(bank$default , bank$y)
table.default
chisq.test(table.default) # Since the p-value is very small, we keep it for further analysis.
table.housing = table(bank$housing, bank$y)
table.housing
chisq.test(table.housing) # Since the p-value is 0.05829, which is greater than the 0.05 significance level, we are
#going to remove the variable for further analysis.
table.loan = table(bank$loan, bank$y)
table.loan
chisq.test(table.loan) # Since the p-value is 0.5787, which is greater than the 0.05 significance level, we are going
#to remove the variable for further analysis.
table.contract = table(bank$contact, bank$y)
table.contract
chisq.test(table.contract) # Since the p-value is very small, we keep it for further analysis.
table.month = table(bank$month, bank$y)
table.month
chisq.test(table.month) # Since the p-value is very small, we keep it for further analysis.
table.day_of_week = table(bank$day_of_week, bank$y)
table.day_of_week
chisq.test(table.day_of_week) # Since the p-value is very small, we keep it for further analysis.
glm.campaign = glm(y~campaign, data = bank, family = binomial)
summary(glm.campaign) # Since the p-value is very small, we keep it for further analysis.
glm.pdays = glm(y~pdays, data = bank, family = binomial)
summary(glm.pdays) # Since the p-value is very small, we keep it for further analysis.
glm.previous = glm(y~previous, data = bank, family = binomial)
summary(glm.previous) # Since the p-value is very small, we keep it for further analysis. 
table.poutcome = table(bank$poutcome, bank$y)
table.poutcome
chisq.test(table.poutcome) # Since the p-value is very small, we keep it for further analysis
glm.emp.var.rate = glm(y~emp.var.rate, data = bank, family = binomial)
summary(glm.emp.var.rate) #Since the p-value is very small, we keep it for further analysis
glm.cons.price.idx = glm(y~cons.price.idx, data = bank, family = binomial)
summary(glm.cons.price.idx) #Since the p-value is very small, we keep it for further analysis
glm.cons.conf.idx = glm(y~cons.conf.idx, data = bank, family = binomial)
summary(glm.cons.conf.idx) #Since the p-value is very small, we keep it for further analysis
glm.euribor3m = glm(y~euribor3m, data = bank, family = binomial)
summary(glm.euribor3m) #Since the p-value is very small, we keep it for further analysis
glm.nr.employed = glm(y~nr.employed, data = bank, family = binomial)
summary(glm.nr.employed) #Since the p-value is very small, we keep it for further analysis
# Therefore, after univariate analysis, we decided to remove housing and loan variable from the dataset.
bank=bank[,-c(6,7)]


install.packages("caret")
library(caret)
library(caTools)
split<- sample.split(bank$y,SplitRatio = 0.7)

train<-subset(bank,split==TRUE)
test<- subset(bank,split == FALSE)

# Logistic Regression
library(leaps)
null<-glm(y~NULL,family=binomial,data=train)
full<-glm(y~.,family=binomial,data=train)
#Step: AIC=16122.32
#nr.employed + month + poutcome + contact + campaign + pdays + emp.var.rate + cons.price.idx + cons.conf.idx + day_of_week

regfit.fwd=step(null,scope=formula(full),direction="forward",k=log(dim(train)[1]-1))
#Step: AIC=16124.5
# nr.employed + month + poutcome + contact + campaign + pdays + emp.var.rate + cons.price.idx + cons.conf.idx
regfit.fwd2= glm(y ~ nr.employed + month + poutcome + contact + campaign + pdays + emp.var.rate + cons.price.idx + cons.conf.idx,data=train,family=binomial)

#Step: AIC=16325.35
#nr.employed + month + poutcome + contact + campaign + pdays + emp.var.rate + cons.price.idx
regfit.fwd3=glm(y~nr.employed + month + poutcome + contact + campaign + pdays + emp.var.rate + cons.price.idx,data=train,family=binomial)
#Next, we will draw ROC curve and calculate the AUC of each of candidates model
#The AUC of regfit.fwd is 0.7990035
library(ROCR)


glm.probs <- predict(regfit.fwd,newdata=train,type="response")
pred <- prediction(glm.probs, train$y)
perfTrain <- performance(pred, "tpr", "fpr")
plot(perfTrain, colorize=F, main="ROC curve1")
unlist(attributes(performance(pred, "auc"))$y.values) 

install.packages("boot")
library(boot)
# Fro cross validation
cv.err <- cv.glm(train,regfit.fwd,)

#The AUC of regfit.fwd2 is 0.798514
glm.probs2 <- predict(regfit.fwd2,newdata=train,type="response")
pred2 <- prediction(glm.probs2, train$y)
perfTrain2 <- performance(pred2, "tpr", "fpr")
plot(perfTrain2, colorize=F, main="ROC curve2")
unlist(attributes(performance(pred2, "auc"))$y.values)

#The AUC of regfit.fwd3 is 0.7978884
glm.probs3 <- predict(regfit.fwd3,newdata=train,type="response")
pred3 <- prediction(glm.probs3, train$y)
perfTrain3 <- performance(pred3, "tpr", "fpr")
plot(perfTrain3, colorize=F, main="ROC curve3")
unlist(attributes(performance(pred3, "auc"))$y.values)
#Obviously the AUC of regfit.fwd is the largest, so we choose this model

summary(regfit.fwd)

#show the coefficients of regfit.fwd
#As we can see, the AUC of the regfit.fwd on Validation Data Set is 0.7770614
glm.probs2_validation <- predict(regfit.fwd2,newdata=test,type="response")
pred2_validation <- prediction(glm.probs2_validation, test$y)
perfTrain2_validation <- performance(pred2_validation, "tpr", "fpr")
plot(perfTrain2_validation, colorize=F, main="ROC curve")
unlist(attributes(performance(pred2_validation, "auc"))$y.values)
#create confusion matrix
y.predict<- predict(regfit.fwd2, newdata=test, se.fit=F, type="response")
pred=c(y.predict>0.5)
Pred=factor(as.factor(pred), labels=c("FALSE","TRUTH"))
Pred
Truth=factor(as.factor(test$y), labels=c("FALSE","TRUTH"))
xtab <- table(Truth, Pred)
xtab 
table(test$y)

# Decision Trees

library(rpart)
library(rpart.plot)

mrpart<-rpart(y ~ nr.employed + month + poutcome + contact + campaign + pdays + emp.var.rate + cons.price.idx + cons.conf.idx + day_of_week, data = train, method= "class", control = rpart.control(cp = 0.0001))

predictrpart<- predict(mrpart,newdata = test, type="class")
#Visualising the tree
prp(mrpart)
confusionMatrix(predictrpart,test$y)

# DT wuth all variables
mrpart<-rpart(y ~ ., data = train, method= "class", control = rpart.control(cp = 0.0001))

predictrpart<- predict(mrpart,newdata = test, type="class")
#Visualising the tree
prp(mrpart)
confusionMatrix(predictrpart,test$y)

#Random Forests
library(randomForest)
mRF<-randomForest(y ~., data = train,nodesize = 25,ntree=200)

predictRF = predict(mRF,newdata= test)
predictRF = ifelse(predictRF>0.5,1,0)
confusionMatrix(predictRF,test$y)
