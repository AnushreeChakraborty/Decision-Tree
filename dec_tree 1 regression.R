head(iris)
any(is.na(iris))
str(iris)
#continuous, so regression tree
library(rpart)
fit<-rpart(Sepal.Length ~ ., method = 'anova', data = iris)
#here entire data set is considered as training data and the for testing we will enter one data and check whether the prediction is correct or not
plot(fit, uniform = TRUE, main="Regression Tree for Sepal Length")
text(fit, use.n = TRUE, cex=.8)
par(mfrow=c(1,2))
rsq.rpart(fit)
#this model improves with each split (performance of each split)
#if it decreases then the problem is of overfitting, the some columns maybe removed for comparing 
#second chart shows xerror wrt no of splits, decreases with each split

#prediction
testData<-data.frame(Species='setosa', Sepal.Width=4, Petal.Length=1.2, Petal.Width=0.3)
predict(fit, testData, method="anova")
#giving the predicted Sepal.Length

#prediction for finding accuracy
library(caTools)
sample<-sample.split(iris$Sepal.Length, SplitRatio=0.70)
train<-subset(iris, sample==TRUE)
test<-subset(iris, sample==FALSE)
predictions<-predict(fit,test,method="anova")
results<-cbind(predictions, test$Sepal.Length)
colnames(results)<-c("pred","real")
results<-as.data.frame(results)
head(results)
SSE<-sum((results$pred - results$real)^2)
SSE
SST<-sum((mean(iris$Sepal.Length) - results$real)^2)
SST
R2<-1-SSE/SST
R2

