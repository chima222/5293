# PCA
studentloan <- read.csv("studentloan.csv",as.is = T)
studentloan <- studentloan[,-1]
studentloan_pca <- studentloan[,-c(1:6)]
scale.studentloan <- scale(studentloan_pca,center = TRUE,scale = TRUE)

pca_loan <- svd(scale.studentloan)
#plot(pca_loan$u[,1:2])
plot(pca_loan$u[,1], pca_loan$u[, 2],pch=16, xlab="First Principle Component", ylab="Second Principle Component" )


pcaX <- princomp(studentloan_pca, cor=T)
biplot(pcaX, cex=0.6)


# after pca, new dataset
studentloan_pca1 <- studentloan[,which(names(studentloan) %in% c("RPY_3YR_RT","PREDDEG","HIGHDEG","CONTROL","REGION","DISTANCEONLY",
                                                                 "PCTFLOAN","UGDS_HISP", "PCTPELL", 
                                                                 "D_PCTPELL_PCTFLOAN", "UGDS", 
                                                                 "MN_EARN_WNE_P8", "UGDS_ASIAN", "MD_EARN_WNE_P10",
                                                                 "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", 
                                                                 "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN",
                                                                 "PAR_ED_PCT_1STGEN"))]








# OLS
# Load data.  
studentloan = read.csv("studentloan.csv", as.is = TRUE)

# Divide dependent and independent dataset
variable.name = c("PREDDEG","HIGHDEG","CONTROL","REGION","DISTANCEONLY", "PCTFLOAN", "UGDS_HISP", "PCTPELL", "D_PCTPELL_PCTFLOAN", "UGDS", "MN_EARN_WNE_P8", "MD_EARN_WNE_P10", "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN", "UGDS_ASIAN","PAR_ED_PCT_1STGEN")
y = studentloan[, "RPY_3YR_RT"]
x = studentloan[, variable.name]

# Variable selection
library(leaps)
subset.5 = regsubsets(y ~., data = x, nvmax = 5)
summary(subset.5)
subset.6 = regsubsets(y ~., data = x, nvmax = 6)
summary(subset.6)
subset.7 = regsubsets(y ~., data = x, nvmax = 7)
summary(subset.7)

# Cross-validation
set.seed(7)
train.index = sample(1:length(y), 0.8*length(y))

# training set
c.5 = c("INC_PCT_LO", "UGDS_ASIAN", "WDRAW_DEBT_MDN", "PCTPELL", "PAR_ED_PCT_1STGEN")
xset.5 = x[train.index, c.5]
c.6 = c("INC_PCT_LO", "UGDS_ASIAN", "WDRAW_DEBT_MDN", "PCTPELL", "PAR_ED_PCT_1STGEN", "UGDS_HISP")
xset.6 = x[train.index, c.6]
c.7 = c("INC_PCT_LO", "UGDS_ASIAN", "WDRAW_DEBT_MDN", "PCTPELL", "PAR_ED_PCT_1STGEN", "UGDS_HISP", "UGDS_MEN")
xset.7 = x[train.index, c.7]
y.train = y[train.index]

# hold out sample
hxset.5 = x[-train.index, c.5]
hxset.6 = x[-train.index, c.6]
hxset.7 = x[-train.index, c.7]
hyset = y[-train.index]

cv = function(xset, yset, k){
  nums = 1:length(yset)
  fold = sample(nums%%k, replace = FALSE)
  mse = rep(NA, k)
  r.squared = rep(NA, k)
  for (j in 1:k) {
    x.test  = xset[fold == j-1, ]
    y.test = yset[fold == j-1]
    x.train = xset[fold != j-1, ]
    y.train = yset[fold != j-1]
    y.pre = predict(lm(y.train ~ ., data = x.train), newdata = x.test)
    mse[j] = mean((y.test - y.pre)^2)
    ssr = sum((y.pre - mean(y.test))^2)
    sst = sum((y.test - mean(y.test))^2)
    r.squared[j] = ssr/sst
  }
  mean.mse = mean(mse)
  mean.r.squared = mean(r.squared)
  return(c(mean.mse, mean.r.squared))
}

cv(xset.5, y.train, 8)
cv(xset.6, y.train, 8)
cv(xset.7, y.train, 8)

# performance of training sample
(train.mse = summary(lm(y.train ~., data = xset.7))$sigma^2)
(train.r.squared = summary(lm(y.train ~., data = xset.7))$r.squared)

# performance of hold out sample
y.pre = predict(lm(y.train ~ ., data = xset.7), newdata = hxset.7)
(mse = mean((hyset - y.pre)^2))
ssr = sum((y.pre - mean(hyset))^2)
sst = sum((hyset - mean(hyset))^2)
(r.squared = ssr/sst)







library(glmnet)
set.seed(7)
studentloan = read.csv("studentloan.csv", as.is = TRUE)
variable.name = c("PREDDEG","HIGHDEG","CONTROL","REGION","DISTANCEONLY", "PCTFLOAN", "UGDS_HISP", "PCTPELL", "D_PCTPELL_PCTFLOAN", "UGDS", "MN_EARN_WNE_P8", "MD_EARN_WNE_P10", "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN", "UGDS_ASIAN","PAR_ED_PCT_1STGEN")
x_lasso = as.matrix(studentloan[, variable.name])
y = studentloan[, "RPY_3YR_RT"]
train.index = sample(1:length(y), 0.8*length(y))
cv_lasso = function(xset, yset, k, lambda){
  nums = 1:length(yset)
  fold = sample(nums%%k, replace = FALSE)
  mse = rep(NA, k)
  r.squared = rep(NA, k)
  for (j in 1:k) {
    x.test  = xset[fold == j-1, ]
    y.test = yset[fold == j-1]
    x.train = xset[fold != j-1, ]
    y.train = yset[fold != j-1]
    lasso.mod = glmnet(x = x.train, y = y.train, alpha = 1, lambda = lambda)
    y.pre = predict(lasso.mod, newx = x.test)
    mse[j] = mean((y.test - y.pre)^2)
    ssr = sum((y.pre - mean(y.test))^2)
    sst = sum((y.test - mean(y.test))^2)
    r.squared[j] = ssr/sst
  }
  mean.mse = mean(mse)
  mean.r.squared = mean(r.squared)
  coef = predict(lasso.mod, type = "coefficients", s = lambda)
  return(list(mean.mse, mean.r.squared, coef))
}

lambda = seq(from = 0, to = 3, by = 0.1)
mse = rep(NA, length(lambda))
for(i in 1:length(lambda)) mse[i] = cv_lasso(x_lasso[train.index, ], y[train.index], 8, lambda[i])[1]
plot(lambda, mse)

coef.num = rep(NA, length(lambda))
for(i in 1:length(lambda)) coef.num[i] = sum(cv_lasso(x_lasso[train.index, ], y[train.index], 8, lambda[i])[3][[1]][, 1] != 0)
plot(lambda, coef.num)

lasso.mod = glmnet(x = x_lasso[train.index, ], y = y[train.index], alpha = 1, lambda = 1.1)
predict(lasso.mod, type = "coefficients", s = 1.1)

# performance of training sample
y.pre.train = predict(lasso.mod, newx = x_lasso[train.index, ], s = 1.1)
(mse = mean((y[train.index] - y.pre.train)^2))
ssr = sum((y.pre.train - mean(y[train.index]))^2)
sst = sum((y[train.index] - mean(y[train.index]))^2)
(r.squared = ssr/sst)

# performance of hold out sample
hxset = x_lasso[-train.index, variable.name]
hyset = y[-train.index]
y.pre = predict(lasso.mod, newx = hxset, s = 1)
(mse = mean((hyset - y.pre)^2))
ssr = sum((y.pre - mean(hyset))^2)
sst = sum((hyset - mean(hyset))^2)
(r.squared = ssr/sst)






studentloan <- read.csv("studentloan.csv",as.is = T)
studentloan <- studentloan[,-1]
studentloan <- cbind(studentloan$RPY_3YR_RT,studentloan[,-which(names(studentloan) %in% c("RPY_3YR_RT"))])
colnames(studentloan)[1] <- c("RPY_3YR_RT")
studentloan[,2] <- as.character(studentloan[,2])
studentloan[,3] <- as.character(studentloan[,3])
studentloan[,4] <- as.character(studentloan[,4])
studentloan[,5] <- as.character(studentloan[,5])
studentloan[,6] <- as.character(studentloan[,6])


write.csv(studentloan,file = "studentloan.csv")

library("rpart")
library("rpart.plot")

#PCA data

studentloan_pca1 <- studentloan[,which(names(studentloan) %in% c("RPY_3YR_RT","PREDDEG","HIGHDEG","CONTROL","REGION","DISTANCEONLY",
                                                                 "PCTFLOAN","UGDS_HISP", "PCTPELL", 
                                                                 "D_PCTPELL_PCTFLOAN", "UGDS", 
                                                                 "MN_EARN_WNE_P8", "UGDS_ASIAN", "MD_EARN_WNE_P10",
                                                                 "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", 
                                                                 "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN",
                                                                 "PAR_ED_PCT_1STGEN"))]
set.seed(7)
foldnum <- sample(nrow(studentloan_pca1),0.8*nrow(studentloan_pca1))
train_tree <- studentloan_pca1[foldnum,]
test_tree <- studentloan_pca1[-foldnum,]



#First model, all variables
treemodel1 <- rpart(RPY_3YR_RT~., data = train_tree,method = "anova",control = rpart.control(minbucket = 20,maxdepth = 8,cp=0.01))
summary(treemodel1)



#model 2 after variable importance

studentloan_tree <- studentloan_pca1[,which(names(studentloan_pca1) %in% 
                                              c("RPY_3YR_RT","INC_PCT_LO","DEP_STAT_PCT_IND","PAR_ED_PCT_1STGEN",
                                                "HIGHDEG","AGE_ENTRY","PREDDEG"))] 


foldnum <- sample(nrow(studentloan_tree),0.8*nrow(studentloan_tree))
train5 <- studentloan_tree[foldnum,]
test5 <- studentloan_tree[-foldnum,]
xset_train5 <- train5[,-1]
yset_train5 <- train5[,1]

# Without tuning
treemodel2 <- rpart(RPY_3YR_RT~., data = train5,method = "anova",control = rpart.control(minbucket = 20,maxdepth = 8,cp=0.01))
summary(treemodel2)




cv <- function(xset, y, k,depth,cp){
  nums <- (1:nrow(xset)) %% k + 1
  fold = sample(nums)
  mse = rep(NA, k)
  r.squared = rep(NA, k)
  for (j in 1:k) {
    x.test  = xset[fold == j, ]
    y.test = y[fold == j]
    x.train = xset[fold != j, ]
    y.train = y[fold != j]
    model <- rpart(y.train~., data = x.train,method = "anova",
                   control = rpart.control(minbucket = 20,maxdepth = depth,cp=cp))
    y.pre = predict(model, newdata = x.test)
    mse[j] = mean((y.test - y.pre)^2)
    ssr = sum((y.pre - mean(y.test))^2)
    sst = sum((y.test - mean(y.test))^2)
    r.squared[j] = ssr/sst
  }
  mean.mse = mean(mse)
  mean.r.squared = mean(r.squared)
  return(c(mean.mse, mean.r.squared,depth))
}


#tune tree depth
set.seed(8)

outcome_depth1 <- matrix(NA,10,3)
for(i in 1:10){
  outcome_depth1[i,] <- cv(xset = xset_train5,y = yset_train5,k = 7,depth = i,cp = 0.001)
  
}

which.min(outcome_depth1[,1])
#The optimal is 8

#The tree
library(ggplot2)
depth <- data.frame(outcome_depth1)
ggplot(data = depth) +
  geom_smooth(mapping = aes(x=X3, y=X1))+
  labs(title = "Regression tree tuned by depth", x = "depth", y = "Mean Squared Error")


#treedepth=8



#tune cp
outcome_cp <- matrix(NA,10,3)
seq_cp <- seq(0.001,0.01,0.001)

for(i in 1:10){
  outcome_cp[i,] <- cv(xset = xset_train5,y = yset_train5,k = 7,depth = 8,cp = seq_cp[i])
  
}

which.min(outcome_cp[,1])

#Choose CP=0.001
#model 4 tune parameter on all training
treemodel3 <- rpart(RPY_3YR_RT~., data = train5,method = "anova",control = rpart.control(minbucket = 20,maxdepth = 8,cp=0.001))
summary(treemodel6)
rpart.plot(treemodel6)

#predict outcome

#default

#after tuning, predict
predict_test1 <- predict(treemodel3,test5)

#train sample
predict_train <- predict(treemodel3,train5)
mse_train <- sum((predict_train-train5[,1])^2)/nrow(train5)
print(mse_train)
rsquare_train <- 1-sum((predict_train-train5[,1])^2)/sum((train5[,1]-mean(train5[,1]))^2)
print(rsquare_train)


#test sample
mse_tree1 <- sum((predict_test1-test5[,1])^2)/nrow(test5)

rsquare_tree1 <- 1-sum((predict_test1-test5[,1])^2)/sum((test5[,1]-mean(test5[,1]))^2)


outcome_tree <- cbind(predict_test1,test5[,1])
colnames(outcome_tree) <-c("Predicted default status","True default status") 

#turning numeric value to class
change_value <- function(threshold,data){
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      if(data[i,j] <= threshold){
        data[i,j] <- 1 #default
      }
      if(data[i,j] > threshold){
        data[i,j] <- 0 
      }
    }
  }
  return(data)
}

outcome_tree5 <- change_value(threshold = quantile(studentloan$RPY_3YR_RT,probs = 0.6) ,data = outcome_tree)
outcome_tree5 <- data.frame(apply(outcome_tree5,2,as.factor))
#generate confounding table
table_quantile60 <- table(outcome_tree5)
table_quantile60







##do random forest with the variable names selected from Lasso


library(randomForest)
##deal with the dataset and separate into
studentloan<-read.csv("studentloan.csv")
studentloan1<-studentloan[,-1]

set.seed(7)

studentloan1[,1]<-as.character(studentloan1[,1])
studentloan1[,2]<-as.character(studentloan1[,2])
studentloan1[,3]<-as.character(studentloan1[,3])
studentloan1[,4]<-as.character(studentloan1[,4])
studentloan1[,5]<-as.character(studentloan1[,5])

var_name<-c("PCTFLOAN", "UGDS_HISP","PCTPELL", "D_PCTPELL_PCTFLOAN", "UGDS", "MN_EARN_WNE_P8", "UGDS_ASIAN", "MD_EARN_WNE_P10", "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN","PAR_ED_PCT_1STGEN","RPY_3YR_RT")


studentloan1<-cbind(studentloan1[,1:5],studentloan1[,var_name])
samp<-sample(nrow(studentloan1),0.8*nrow(studentloan1))
train<-studentloan1[samp,]
test<-studentloan1[-samp,]



var_name_lasso<-c("UGDS_HISP","PCTPELL","MN_EARN_WNE_P8","UGDS_ASIAN","INC_PCT_LO","WDRAW_DEBT_MDN","PAR_ED_PCT_1STGEN","RPY_3YR_RT")
studentloan2<-cbind(studentloan1[,1:5],studentloan1[,var_name_lasso])

samp2<-sample(nrow(studentloan2),0.2*nrow(studentloan2))
test2<-studentloan2[samp2,]
train2<-studentloan2[-samp2,]
randomforest<-randomForest(formula=RPY_3YR_RT~.,data=train2,mtry=4)
summary(randomforest)
yhat_rf2<-predict(randomforest,test2)
sum((yhat_rf2-test2[,"RPY_3YR_RT"])^2)/nrow(test2)
sse_randomforest<-sum((yhat_rf2-test2[,"RPY_3YR_RT"])^2)/nrow(test2)
rsquare_randomforest<-1-sum((yhat_rf2-test2[,"RPY_3YR_RT"])^2)/sum((test2[,"RPY_3YR_RT"]-mean(test2[,"RPY_3YR_RT"]))^2)
importance(randomforest)
varImpPlot(randomforest)

#MSE and R^2 for training data
yhat_rf3<-predict(randomforest,train2)
sse_randomforest_train<-sum((yhat_rf3-train2[,"RPY_3YR_RT"])^2)/nrow(train2)##training MSE
rsquare_randomforest_train<-1-sum((yhat_rf3-train2[,"RPY_3YR_RT"])^2)/sum((train2[,"RPY_3YR_RT"]-mean(train2[,"RPY_3YR_RT"]))^2)  ##training R^2



mse_tree1 <- sum((predict_test1-test5[,1])^2)/nrow(test5)   ##test MSE

rsquare_tree1 <- 1-sum((predict_test1-test5[,1])^2)/sum((test5[,1]-mean(test5[,1]))^2)   ##test R^2







##do logistic and LDA/QDA
df <- read.csv("studentloan.csv")
library(MASS)
library(glmnet)
df1 = df[-1]
variable1 = c("UGDS_HISP","PCTPELL","MN_EARN_WNE_P8","UGDS_ASIAN","INC_PCT_LO","WDRAW_DEBT_MDN","PAR_ED_PCT_1STGEN","RPY_3YR_RT")

df2 = as.matrix(df[c(variable1)])



#sample train/test 80/20
smp_size = floor(nrow(df2)*0.8)
set.seed(7)
train_ind = sample(seq_len(nrow(df2)),size = smp_size)
train = df2[train_ind,]
test = df2[-train_ind,]
X_train = train[,-8]
y_train = train[,8]
X_test = test[,-8]
y_test = test[,8]

#set threshold

set_threshold <- function(input,threshold){
  value = quantile(input,threshold) 
  for (i in 1:length(input)){
    if (input[i] >= value){
      input[i] = 1}
    else{
      input[i] = 0
    }
  }
  return(input)
}

#change y value
y_train<- set_threshold(y_train,0.6)
y_test<- set_threshold(y_test,0.6)



#logistic
cvfit = cv.glmnet(X_train,y_train,family = "binomial",type.measure = "auc")

#train accuracy
glm.pred.train = predict(cvfit,X_train,s = "lambda.min",type = "class")
glm.train.accy = mean(predict(cvfit,X_train,s = "lambda.min",type = "class")==y_train)
#test accuracy
glm.pred.test = predict(cvfit,X_test,s = "lambda.min",type = "class")
glm.test.accy = mean(predict(cvfit,X_test,s = "lambda.min",type = "class")==y_test)


#lda
lda.fits<-lda(X_train,y_train)
summary(lda.fits)
#lda train
lda.pred.train = predict(lda.fits,X_train)
table(lda.pred.train$class,y_train)
lda.train.accy<-mean(lda.pred.train$class == as.matrix(y_train))
#lda test
lda.pred.test = predict(lda.fits, X_test)
table(lda.pred.test$class,y_test)
lda.test.accy<-mean(lda.pred.test$class == as.matrix(y_test))


#qda
qda.fits<-qda(X_train,y_train)
summary(qda.fits)
#qda train
qda.pred.train = predict(qda.fits,X_train)
table(qda.pred.train$class,y_train)
qda.train.accy<-mean(qda.pred.train$class == y_train)
#qda test
qda.pred.test = predict(qda.fits,X_test)
table(qda.pred.test$class,as.matrix(y_test))
qda.test.accy<-mean(qda.pred.test$class == y_test)


library(ggplot2)

rocplot = function(pred,truth){
  predob = prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf)
}

glm.test.err = 1 - glm.test.accy
lda.test.err = 1 - lda.test.accy
qda.test.err = 1 - qda.test.accy

glm.train.err = 1 - glm.train.accy
lda.train.err = 1 - lda.train.accy
qda.train.err = 1 - qda.train.accy


#error rate
log.test.err = 1 - glm.test.accy
lda.test.err = 1 - lda.test.accy
qda.test.err = 1 - qda.test.accy

log.train.err = 1 - glm.train.accy
lda.train.err = 1 - lda.train.accy
qda.train.err = 1 - qda.train.accy


#AUC ROC
library(pROC)
library(ROCR)
glm.roc.test = as.numeric(glm.pred.test)
lda.roc.test = as.numeric(lda.pred.test$class)
qda.roc.test = as.numeric(qda.pred.test$class)

glm.roc.train = as.numeric(glm.pred.train)
lda.roc.train = as.numeric(lda.pred.train$class)
qda.roc.train = as.numeric(qda.pred.train$class)

glm.auc.test = auc(y_test,glm.roc.test)
lda.auc.test = auc(y_test,lda.roc.test)
qda.auc.test = auc(y_test,qda.roc.test)

glm.auc.train = auc(y_train,glm.roc.train)
lda.auc.train = auc(y_train,lda.roc.train)
qda.auc.train = auc(y_train,qda.roc.train)






##do KNN for it, use variable from lasso

studentloan<-read.csv("studentloan.csv")
studentloan1<-studentloan[,-1]

set.seed(7)

studentloan1[,1]<-as.character(studentloan1[,1])
studentloan1[,2]<-as.character(studentloan1[,2])
studentloan1[,3]<-as.character(studentloan1[,3])
studentloan1[,4]<-as.character(studentloan1[,4])
studentloan1[,5]<-as.character(studentloan1[,5])

var_name<-c("PCTFLOAN", "UGDS_HISP","PCTPELL", "D_PCTPELL_PCTFLOAN", "UGDS", "MN_EARN_WNE_P8", "UGDS_ASIAN", "MD_EARN_WNE_P10", "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN","PAR_ED_PCT_1STGEN","RPY_3YR_RT")


studentloan1<-cbind(studentloan1[,1:5],studentloan1[,var_name])
samp<-sample(nrow(studentloan1),0.8*nrow(studentloan1))
train<-studentloan1[samp,]
test<-studentloan1[-samp,]





library(DMwR)

var_name_las<-c("UGDS_HISP","PCTPELL","MN_EARN_WNE_P8","UGDS_ASIAN","INC_PCT_LO","WDRAW_DEBT_MDN","PAR_ED_PCT_1STGEN")

y_col<-which(colnames(train)=="RPY_3YR_RT")

x_knn_train<-train[,-c(1,2,3,4,5,y_col)]
x_knn_test<-test[,-c(1,2,3,4,5,y_col)]

x_knn_train<-x_knn_train[,var_name_las]
x_knn_test<-x_knn_test[,var_name_las]

y_knn_train<-train[,y_col]
y_knn_test<-test[,y_col]

thre_hold=0.6
thre_val=quantile(studentloan[,which(colnames(train)=="RPY_3YR_RT")],0.6)
y_knn_tr<-((y_knn_train>=thre_val)-1)*-1
y_knn_te<-((y_knn_test>=thre_val)-1)*-1

trainKNN<-cbind(y_knn_tr,x_knn_train)
colnames(trainKNN)[1]<-"y"
testKNN<-cbind(y_knn_te,x_knn_test) 
colnames(testKNN)[1]<-"y"

tune_knn<-function(totk,traindata,k=5){
  n_row<-nrow(traindata)
  idx<-seq(1,n_row)
  mod_val<-idx%%k+1
  mod_val<-sample(mod_val,replace = FALSE)
  tune_err<-rep(0,totk)
  for (i in 1:totk){
    cv_error<-rep(0,k)
    for (j in 1:k){ 
      train_cv<-traindata[mod_val!=j,]
      test_cv<-traindata[mod_val==j,]
      knnmodel<-kNN(form=y~.,train_cv,test_cv,k=i)
      temp<-table(test_cv[,"y"],knnmodel)
      cv_error[j]<-sum(temp[1,2],temp[2,1])
    }
    tune_err[i]<-mean(cv_error)
  }
  return(which.min(tune_err))
}
best_k<-tune_knn(totk=10,traindata=trainKNN)  ##we get k=7 with CV
knnmodel<-kNN(form=y~.,trainKNN,testKNN,k=best_k)

a<-table(testKNN[,"y"],knnmodel) ##counfounding table
sum(a[1,2],a[2,1])/sum(a) ##error rate of KNN method

library("pROC")
knnmodel<-as.numeric(knnmodel)
auc(y_knn_te,knnmodel)







library(e1071)
library(pROC)
set.seed(7)
studentloan = read.csv("studentloan.csv", as.is = TRUE)
variable.name = c("PREDDEG","HIGHDEG","CONTROL","REGION","DISTANCEONLY", "PCTFLOAN", "UGDS_HISP", "PCTPELL", "D_PCTPELL_PCTFLOAN", "UGDS", "MN_EARN_WNE_P8", "MD_EARN_WNE_P10", "UGDS_MEN", "UGDS_WOMEN", "INC_PCT_LO", "DEP_STAT_PCT_IND", "AGE_ENTRY", "WDRAW_DEBT_MDN","DEP_DEBT_MDN","MD_INC_DEBT_MDN", "UGDS_ASIAN","PAR_ED_PCT_1STGEN")
y = studentloan[, "RPY_3YR_RT"]
x = studentloan[, variable.name]
train.index = sample(1:length(y), 0.8*length(y))
threshold = 0.6
y = studentloan[, "RPY_3YR_RT"]
newy = as.factor(y >= quantile(y, threshold))
y.train = newy[train.index]
hyset = newy[-train.index]
baymod = naiveBayes(y.train ~ ., data = studentloan[train.index, variable.name])

# performance of training sample
y.pre.train = predict(baymod, studentloan[train.index, variable.name])

# loss function
(misclasification.rate.train = mean(y.train != y.pre.train))
table(y.pre.train, y.train)

# AUC
(bayes.auc.train = auc(as.numeric(y.train)-1, as.numeric(y.pre.train)-1))

# performance of hold out sample
y.pre = predict(baymod, studentloan[-train.index, variable.name])

# loss function
(misclasification.rate = mean(hyset != y.pre))
table(hyset, hyset)

# AUC
(bayes.auc.test = auc(as.numeric(hyset)-1, as.numeric(y.pre)-1))













