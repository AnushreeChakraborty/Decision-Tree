data<-read.csv("D:/R/data sets/Admission_Predict.csv")
head(data)
summary(data)
str(data)
any(is.na(data))
dim(data)

data$University.Rating<-as.factor(data$University.Rating)
data$Research<-as.factor(data$Research)
str(data)
data<-data[,-1]
data[1:5,]

library(ggplot2)
ggplot(data, aes(Chance.of.Admit))+geom_bar(fill='red')
ggplot(data, aes(x=University.Rating))+geom_bar()

library(caTools)
sample<-sample.split(data$Chance.of.Admit, SplitRatio=0.70)
train<-subset(data, sample==TRUE)
test<-subset(data, sample==FALSE)

library(rpart)
fit<-rpart(Chance.of.Admit ~ ., method = 'anova', data = train)
plot(fit, uniform = TRUE, main="Regression Tree for Prediction of Admission")
text(fit, use.n = TRUE, cex=.8)
par(mfrow=c(1,2))
rsq.rpart(fit)

predictions<-predict(fit,test,method="anova")
results<-cbind(predictions, test$Chance.of.Admit)
colnames(results)<-c("pred","real")
results<-as.data.frame(results)
head(results)

SSE<-sum((results$real - results$pred)^2)
SSE
SST<-sum(((results$real - mean(data$Chance.of.Admit))^2))
SST
R2<-(1-(SSE/SST))
R2

#k-folds cross validation
library(caret)
data_ctrl<-trainControl(method = 'cv', number = 10)
model_caret<-train(Chance.of.Admit  ~ ., 
                   data=data, 
                   trControl=data_ctrl, 
                   method='rpart',
                   na.action=na.pass)
model_caret
a<-model_caret$resample
mean(a$Rsquared)

