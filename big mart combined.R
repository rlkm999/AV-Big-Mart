#clearing environment
rm(list = ls())

#path
setwd("G:/AV/big mart")

#loading libraries

#install.packages("mice")
#install.packages("VIM")
#install.packages("boruta")

library(caret)
library(DMwR)
library(psych)
library(randomForest)
library(corrplot)
library(mice)
library(VIM)
library(ggplot2)
library(xgboost)
library(Boruta)
library(e1071)
library(gbm)

#read data
train_data<-read.csv("Train_UWu5bXk.csv")
test_data<-read.csv("Test_u94Q5KV.csv")

#data preprocessing
sum(is.na(train_data))
sum(is.na(test_data))
colSums(is.na(train_data))
colSums(is.na(test_data))

qplot(final$Item_Weight,data = final)
qplot(final$Item_Visibility,data = final)

train_target<-subset(train_data,select=c(12))
train_data$Item_Outlet_Sales<-NULL

final<-rbind(train_data,test_data)

colSums(is.na(final))

final$Item_Weight[is.na(final$Item_Weight)]<-mean(final$Item_Weight,na.rm = T)

summary(final)

final$Item_Fat_Content[final$Item_Fat_Content=="LF"]<-"Low Fat"
final$Item_Fat_Content[final$Item_Fat_Content=="low fat"]<-"Low Fat"
final$Item_Fat_Content[final$Item_Fat_Content=="reg"]<-"Regular"

final$Item_Fat_Content<-as.factor(final$Item_Fat_Content)
table(final$Item_Fat_Content)

final$Item_Fat_Content<-factor(final$Item_Fat_Content,levels = c("Low Fat","Regular"))

ggplot(final,aes(x=final$Outlet_Identifier,y=final$Item_Weight))+geom_boxplot()

#droplevels(final$Item_Fat_Content)
str(final)

table(final$Item_Visibility)
final$Item_Visibility[(final$Item_Visibility)==0]<-mean(final$Item_Visibility,na.rm = T)

#final$Item_Visibility<-mice(final,m=1)

boxplot(final$Item_Visibility)
f1<-function(x1)
{
  q<-quantile(x1,c(0.05,0.95))
  x1[x1>q[2]]<-q[2]
  x1
}

f1(final$Item_Visibility)->final$Item_Visibility

summary(final)

table(final$Outlet_Size)

final$Outlet_Size[final$Outlet_Size==""]<-"Medium"

table(final$Outlet_Size)
final$Outlet_Size<-factor(final$Outlet_Size,levels = c("High","Medium","Small"))

#final$Outlet_Establishment_Year<-as.factor(final$Outlet_Establishment_Year)
final$Outlet_Establishment_Year<-as.numeric(final$Outlet_Establishment_Year)

str(final)

final$Outlet_Establishment_Year<-2013-final$Outlet_Establishment_Year
final$Outlet_Establishment_Year<-as.factor(final$Outlet_Establishment_Year)
table(final$Item_Type)

levels(final$Item_Fat_Content)<-c(levels(final$Item_Fat_Content),"None")

final[which(final$Item_Type=="Health and Hygiene"),]$Item_Fat_Content<-"None"
final[which(final$Item_Type=="Household"),]$Item_Fat_Content<-"None"
final[which(final$Item_Type=="Others"),]$Item_Fat_Content<-"None"

table(final$Outlet_Type)

levels(final$Outlet_Type)<-c(levels(final$Outlet_Type),"Supermarket")

final$Outlet_Type[final$Outlet_Type=="Supermarket Type1"]<-"Supermarket"
final$Outlet_Type[final$Outlet_Type=="Supermarket Type2"]<-"Supermarket"
final$Outlet_Type[final$Outlet_Type=="Supermarket Type3"]<-"Supermarket"

final$Outlet_Type<-factor(final$Outlet_Type,levels = c("Grocery Store","Supermarket"))
final$Outlet_Type<-as.factor(final$Outlet_Type)

str(final)

#write.csv(final,file = "final.csv",row.names = F)
#write.csv(train_target,file = "train_target.csv",row.names = F)
#levels(final$Item_Type)<-c(levels(final$Item_Type),"liquids")
#final$Item_Type[which(final$Item_Type=="Baking Goods"&final$Item_Type=="Breads"&final$Item_Type=="Breakfast"&final$Item_Type=="Dairy"&final$Item_Type=="Frozen Foods"&final$Item_Type=="Fruits and Vegetables"&final$Item_Type=="Meat"&final$Item_Type=="Seafood"&final$Item_Type=="Snack Foods"&final$Item_Type=="Starchy Foods"),]$Item_Type<-"solids"
#final[which(final$Item_Type=="Canned"),]$Item_Type<-"liquids"
#num_final<-subset(final,select=c(2,4,6,8))
#corrplot(cor(num_final))
#pairs.panels(num_final)

train<-final[1:8523,]
test<-final[8524:14204,]

train<-cbind(train,train_target)
train$Item_Identifier<-NULL
train$Outlet_Identifier<-NULL

Item_Identifier<-test$Item_Identifier
Item_Identifier<-as.data.frame(Item_Identifier)
test$Item_Identifier<-NULL

Outlet_Identifier<-test$Outlet_Identifier
Outlet_Identifier<-as.data.frame(Outlet_Identifier)
test$Outlet_Identifier<-NULL

qplot(sqrt(train$Item_Outlet_Sales),data = train)

#train$Item_Outlet_Sales<-log(train$Item_Outlet_Sales)

#train$Item_Outlet_Sales<-sqrt(train$Item_Outlet_Sales)
str(train)
#model building

rf<-randomForest(train$Item_Outlet_Sales~.,data = train,importance=T,ntree=100,cp=0.01)

Item_Outlet_Sales<-(predict(rf,test))
Item_Outlet_Sales<-as.data.frame(Item_Outlet_Sales)

i<-predict(rf,train)

rf_rmse<-RMSE(i,train$Item_Outlet_Sales)
rf_rmse


############

library(rpart)

rp<-rpart(train$Item_Outlet_Sales~.,data = train,cp=.001)
rp

h<-predict(rp,train)

rp_rmse<-RMSE(h,train$Item_Outlet_Sales)
rp_rmse

Item_Outlet_Sales<-predict(rp,test)
Item_Outlet_Sales<-as.data.frame(Item_Outlet_Sales)


##############
traincv<-trainControl(method = "repeatedcv",number = 3,repeats = 3)

gbm<-train(train[,-10],train$Item_Outlet_Sales,method="gbm",trControl=traincv,tuneLength=10)

f<-predict(gbm,train)

Item_Outlet_Sales<-predict(gbm,test)
Item_Outlet_Sales<-(Item_Outlet_Sales)
Item_Outlet_Sales<-as.data.frame(Item_Outlet_Sales)

#################

xgb.ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 5, search='random', allowParallel=T)


#install.packages("caretEnsemble")
library(caretEnsemble)

methodList=c('glm', 'gbm', 'svmRadial', 'xgbLinear')


model_list <- caretList(Item_Outlet_Sales~., data=train,
                        trControl=traincv,
                        methodList=methodList)

ensemble<-caretEnsemble(model_list)
summary(ensemble)

c<-predict(ensemble,train)

Item_Outlet_Sales<-predict(ensemble, newdata=test)

Item_Outlet_Sales <- as.data.frame(Item_Outlet_Sales)
##################################



#svm.model<-svm(train$Item_Outlet_Sales~.,data = train)
#svm.model

#Item_Outlet_Sales<-predict(svm.model,test)
#Item_Outlet_Sales<-as.data.frame(Item_Outlet_Sales)


#data(agaricus.train, package='xgboost')
#train1 <- agaricus.train
#dtrain <- xgb.DMatrix(as.matrix(train), label=train_target)

# #xgb_cv<-xgb.cv(data = dtrain, nrounds = 3, nthread = 2, nfold = 5, metrics = "rmse",
#                max_depth = 3, eta = 1, objective = "reg:linear")

#summary(xgb_cv)
#xgb_cv$params
#xgb_cv$niter
#xgb_cv$evaluation_log

# 
#  #xgb.fit<-xgboost(data = data.matrix(train),
#         label = data.matrix(train$Item_Outlet_Sales),
#         booster = "gbtree", 
#         objective = "reg:linear", 
#         max.depth = 5, 
#         eta = 0.5, 
#         nthread = 2, 
#         nround = 2,
#         min_child_weight = 1,
#         subsample = 0.5,
#         colsample_bytree = 1,
#         num_parallel_tree = 1)

#Item_Outlet_Sales<-(predict(xgb.fit,data.matrix(test)))

#Item_Outlet_Sales<-as.data.frame(Item_Outlet_Sales)

#xgb.tune <-train(Item_Outlet_Sales~.,data = train,method="knn",trControl=xgb.ctrl,tuneLength=20,verbose=T,metric="RMSE")

#xgb.tune$results
#xgb.tune$metric
#xgb.tune$pred
#Item_Outlet_Sales<-predict(xgb.tune,test)


rmse<-RMSE(c,(train$Item_Outlet_Sales))
rmse
#############################################
 


################################################
id<-cbind(Item_Identifier,Outlet_Identifier)

samplesubmission<-cbind(id,Item_Outlet_Sales)
write.csv(samplesubmission,file = "SampleSubmission.csv",row.names = F)
