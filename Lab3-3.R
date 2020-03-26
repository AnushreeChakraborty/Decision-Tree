df<-read.csv("D:/R/data sets/heart.csv")
head(df)
str(df)
summary(df)
any(is.na(df))
dim(df)

colnames(df)
library(dplyr)
df<-df %>%
  rename(age=ï..age)
colnames(df)
df$sex<-as.factor(df$sex)
df$cp<-as.factor(df$cp)
df$fbs<-as.factor(df$fbs)
df$restecg<-as.factor(df$restecg)
df$exang<-as.factor(df$exang)
df$slope<-as.factor(df$slope)
df$ca<-as.factor(df$ca)
df$thal<-as.factor(df$thal)
df$target<-as.factor(df$target)
str(df)
summary(df)

library(ggplot2)
#ggplot(df, aes(age))+geom_bar()
ggplot(df, aes(age))+geom_bar(aes(fill=target))
ggplot(df, aes(age))+geom_bar(aes(fill=cp))
ggplot(df, aes(fbs))+geom_bar()

library(caTools)
sample<-sample.split(df$target, SplitRatio = 0.70)
train<-subset(df, sample==TRUE)
test<-subset(df, sample==FALSE)

#model
library(rpart)
tree<-rpart(target ~., method = 'class', data = train)
tree.preds<-predict(tree,test)
head(tree.preds)
tree.preds<-as.data.frame(tree.preds)
change<-function(x)
{
  if(x>=0.5)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}
tree.preds$class<-sapply(tree.preds$`1`, change)
head(tree.preds)
tree.preds

#confusion matrix
confusion.matrix<-table(tree.preds$class, test$target)
confusion.matrix
TN<-confusion.matrix[1,1]
FP<-confusion.matrix[1,2]
FN<-confusion.matrix[2,1]
TP<-confusion.matrix[2,2]
print(paste("True Negative = ",TN))
print(paste("False Positive = ",FP))
print(paste("False Negative = ",FN))
print(paste("True Positive = ",TP))

Error<-(FP+FN)/(TP+TN+FP+FN)
Error
Accuracy<-1-Error
Accuracy

library(rpart.plot)
prp(tree)
library(pROC)
test_roc<-roc(test$target ~ tree.preds$class, plot=TRUE, print.auc=TRUE)

#random forest
library(randomForest)
rf.model<-randomForest(target ~., data = train, importance=TRUE)
#prediction
p<-predict(rf.model,test)
p
q<-as.numeric(p)
class(q)
q
change2<-function(x)
{
  if(x==2)
    return(1)
  else
    return(0)
}
q2<-sapply(q,change2)
q2

test_roc<-roc(test$target ~ q2, plot=TRUE, print.auc=TRUE)

c2<-table(q2,test$target)
c2
TN2<-c2[1,1]
FP2<-c2[1,2]
FN2<-c2[2,1]
TP2<-c2[2,2]
print(paste("True Negative = ",TN2))
print(paste("False Positive = ",FP2))
print(paste("False Negative = ",FN2))
print(paste("True Positive = ",TP2))

Error2<-(FP2+FN2)/(TP2+TN2+FP2+FN2)
Error2

Accuracy2<-1-Error2
Accuracy2
