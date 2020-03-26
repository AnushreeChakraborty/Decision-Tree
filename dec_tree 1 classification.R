library(ISLR)
head(College)
dim(College)
any(is.na(df))
str(College)

#private-tells whether college or uni is public or private(target variale). 
#F.Undergrad- full time undergrad
#P.Undergrad- part time undergrad 
#Room.Board- other facilities uni provides 
df<-College

library(ggplot2)
ggplot(df,aes(Room.Board,Grad.Rate))+geom_point(aes(color=Private))
#private provides more facilities which might indicate the reason for high grad.rate

ggplot(df,aes(F.Undergrad))+geom_histogram(aes(fill=Private),color='black',bins=50)
#private uni have 5000 almost students, very less have more than 10000, and most of them are public

ggplot(df,aes(Grad.Rate))+geom_histogram(aes(fill=Private),color='black',bins=50)
#whether Grad.Rate changes with unversity type
#private Universities have more Grad. Rate
#one wrong value, Result cannot be more than 100. It can be maximum 100

subset(df,Grad.Rate>100)
#changing the value of that particular college's Grad. Rate to 100
df['Cazenovia College','Grad.Rate']<-100

library(caTools)
sample<-sample.split(df$Private, SplitRatio = 0.70)
train<-subset(df, sample==TRUE)
test<-subset(df, sample==FALSE)

#model building
install.packages('rpart')
library(rpart)
tree<-rpart(Private ~., method = 'class', data = train)
tree.preds<-predict(tree,test)
head(tree.preds)
#probability values
tree.preds<-as.data.frame(tree.preds)
joiner<-function(x)
{
  if(x>=0.5)
  {
    return('Yes')
  }
  else
  {
    return('No')
  }
}
tree.preds$Private<-sapply(tree.preds$Yes, joiner)
head(tree.preds)
tree.preds

#if you pass No to the function then just the condition will get reversed

#confusion matrix
confusion.matrix<-table(tree.preds$Private, test$Private)
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

#tree
install.packages('rpart.plot')
library(rpart.plot)

prp(tree)

change<-function(x)
{
  if(x=='Yes')
    return(1)
  else
    return(0)
}
test_prob<-sapply(tree.preds$Private,change)
head(test_prob,3)
test_roc<-roc(test$Private ~ test_prob, plot=TRUE, print.auc=TRUE)
#function to create tree
#root is F.Undergraduate, found to be the most relevant
#here we dont have any identifier and all seems relevant so we take all the columns
#which attributes to be considered, what are the conditions, and when to stop
#private is yes and public is no

#random forest
library(randomForest)
rf.model<-randomForest(Private ~., data = train, importance=TRUE)
#rf.model$confusion
#prediction
p<-predict(rf.model,test)
p
#if it is not changed to integer or to factor variable, it causes an error. So converting it is preferable
change<-function(x)
{
  if(x=='Yes')
    return(1)
  else
    return(0)
}
test_prob<-sapply(p,change)
head(test_prob,3)
class(test_prob)
test_roc<-roc(test$Private ~ test_prob, plot=TRUE, print.auc=TRUE)

c2<-table(p,test$Private)
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
library(pROC)
