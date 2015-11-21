#Train Data via SVM
setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")

load(file="DAT_train_data_main.Rda")

library("e1071")
dfms$same=factor(dfms$same)

n_examples=5000
n_features=1000

hist_diff=as.data.frame(hist_diff)
hist_diff$same=dfms$same

hds=hist_diff[1:n_examples, c(1:n_features,ncol(hist_diff))]
hdp=hist_diff[5000:6000, c(1:n_features,ncol(hist_diff))]

#Check that all n-grams have values and remove those that don't
if( sum(colSums(hds[,1:(ncol(hds)-1)])==0)!=0 ) {
  ind_gone=which(colSums(hds[,1:(ncol(hds)-1)])==0)
  hds=hds[,-ind_gone]
  hdp=hdp[,-ind_gone]
}

#Check that all n-grams have variance and remove those that don't
#Otherwise problems with scaling
k=apply(hds[1:(ncol(hds)-1)], 2, var)
if( sum(k==0)>0 ) {
  ind_gone=which(k==0)
  hds=hds[,-ind_gone]
  hdp=hdp[,-ind_gone]
}



#Apply PCa
dat.sc <- scale(hds[,1:(ncol(hds)-1)], center=TRUE, scale=FALSE)

dat.pca=prcomp(dat.sc)

plot(dat.pca,type = "l")
sink(file='pca.txt')
summary(dat.pca)
sink()
n=625 #number of pca components

ndfs=data.frame(dat.pca$x[,1:n])
ndfs$same=hds$same

ndfp=data.frame( predict(dat.pca, newdata=hdp)[,1:n] )
ndfp$same=hdp$same


#Model#1
start <- Sys.time ()
model1 <- svm(same~., data=ndfs)
print(Sys.time () - start)

p=predict(model1,newdata = ndfp )
sum(p==ndfp$same)/nrow(ndfp)*100
table(pred=p, true=ndfp$same)


model2 <- tune.svm(same~., data=ndfs)
#save(model1,res1, file="DAT_model1.Rda")
# 
# #Model#2
# start <- Sys.time ()
# model2 <- svm(same~., data=hds,kernel="linear", cost=0.001)
# print(Sys.time () - start)
# p=predict(model2,newdata = hdp )
# sum(p==hdp$same)/nrow(hdp)*100
# res2=table(pred=p, true=hdp$same)
# save(model2,res2, file="DAT_model2.Rda")
# 
# #Model#3
# start <- Sys.time ()
# model3 <- svm(same~., data=hds)
# print(Sys.time () - start)
# t3=Sys.time () - start
# p=predict(model3,newdata = hdp )
# sum(p==hdp$same)/nrow(hdp)*100
# table(pred=p, true=hdp$same)
# #save(model3,res3, t3, file="DAT_model3.Rda")
# 
# 
# #Model#4
# start <- Sys.time ()
# model4 <- svm(same~., 
#               data=hds, 
#               cost=1, 
#               scale=TRUE,
#               gamma=0.002
#               )
# print(Sys.time () - start)
# 
# 
# #t4=Sys.time () - start
# p=predict(model4,newdata = hdp )
# sum(p==hdp$same)/nrow(hdp)*100
# table(pred=p, true=hdp$same)
# #save(model4,res4,t4, file="DAT_model4.Rda")

library(caret)
library(dplyr)
library(kernlab)



load(file="DAT_train_data_main.Rda")
n_features=ncol(hist_diff)

dfms$same=factor(dfms$same)
hist_diff=as.data.frame(hist_diff)
hist_diff$same=dfms$same


trainIndex=createDataPartition(hist_diff$same,p=.07, list=FALSE)
trainData=hist_diff[trainIndex,]
testData=hist_diff[-trainIndex,]

trainX=trainData[,1:n_features]


#Check for zero columns and remove them
if( sum(colSums(trainX)==0)!=0 ) {
  ind_gone=which(colSums(trainX)==0)
  trainX=trainX[,-ind_gone]
  trainData=trainData[,-ind_gone]
  testData=testData[,-ind_gone]
  n_features=n_features-length(ind_gone)
}

k=apply(trainX, 2, var)
#Check that all n-grams have variance and remove those that don't
#Otherwise problems with scaling
if( sum(k==0)>0 ) {
  ind_gone=which(k==0)
  trainX=trainX[,-ind_gone]
  trainData=trainData[,-ind_gone]
  testData=testData[,-ind_gone]
  n_features=n_features-length(ind_gone)
}



set.seed(1492)

ctrl=trainControl(method = "repeatedcv",
                  repeats=5)#,        # do 5 repititions of cv
                  #summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                  #classProbs=TRUE)

nf=n_features

start <- Sys.time ()
svm.tune=train(x=trainX[,1:nf], y=trainData$same, preProcess=c("center","scale"),
               method="svmLinear", trControl=ctrl)
print(Sys.time () - start)
library(beepr)
beep()
svm.tune

p=predict(svm.tune, newdata=trainX[,1:nf])
confusionMatrix(p, trainData$same)

p=predict(svm.tune, newdata=testData[,1:nf])
confusionMatrix(p, testData$same)





# 
# #Sample Data
# 
# 
# df=data.frame(n1=c(rep(1,1000), rep(0,1000))+rnorm(2000,0,0.4),
#               n2=c(rep(0.1,1000), rep(0.2,1000))+rnorm(2000,0,0.4),
#               y=c(rep(1,1000), rep(0,1000)))
#               
# df$y=factor(df$y)
#        
# trainIndex=createDataPartition(df$y,p=.5, list=FALSE)
# trainData=df[trainIndex,]
# testData=df[-trainIndex,]
# 
# trainX=trainData[,1:2]
# ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
#                      repeats=5,  	    # do 5 repititions of cv
#                      summaryFunction=twoClassSummary,	# Use AUC to pick the best model
#                      classProbs=TRUE)
# 
# grid <- expand.grid(sigma = c(.01, .015, 0.2),
#                     C = c(0.75, 0.9, 1, 1.1, 1.25))
# 
# start <- Sys.time ()
# 
# svm.tune <- train(x=trainX,
#                   y= trainData$y,
#                   method = "svmRadial",
#                   preProc = c("center","scale"),
#                   metric="ROC",
#                   tuneGrid = grid,
#                   trControl=ctrl)
# 
# print(Sys.time () - start)
# beep()
# svm.tune
# 
# p=predict(svm.tune, newdata=testData[,1:2])
# confusionMatrix(p, trainData$y, positive="1")
# sum(p==trainData$y)/nrow(trainData)*100
