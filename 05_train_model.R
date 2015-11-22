#Train Data via SVM
setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")

library(caret)
library(dplyr)
library(kernlab)




load(file="DAT_train_data_main.Rda")
n_features=ncol(hist_diff)

dfms$same=factor(dfms$same)
hist_diff=as.data.frame(hist_diff)
hist_diff$same=dfms$same

#Since this is just the preliminary model fitting, we just use the training data as also test data
trainIndex=createDataPartition(hist_diff$same,p=.1, list=FALSE)
trainData=hist_diff[trainIndex,]
testData=hist_diff[-trainIndex,]

trainX=trainData[,1:n_features]




#Plot Correlation
save(trainX, trainData, file="DAT_report2.Rda")
hist(cor(trainX, as.numeric(trainData$same)),
     breaks=50,
     main="Fig2: Features-Prediction Correlation",
     xlab="Linear Correlation",
     col="red")


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

#ctrl=trainControl(method = "repeatedcv",
                  #repeats=1)#,        # do 5 repititions of cv
                  #summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                  #classProbs=TRUE)

ctrl=trainControl(method="cv",number=2)

nf=n_features

grid <- expand.grid(C = c(0.1,0.5,1),
                    sigma=c(.01,.1,1)
)

start <- Sys.time ()
svm.tune=train(x=trainX[,1:nf], y=trainData$same,# preProcess=c("center","scale"),
               tuneLength = 9,
               method="svmRadial", 
               tuneGrid = grid,
               trControl=ctrl,#method="rf",
               prox=TRUE,allowParallel=TRUE)
print(Sys.time () - start)
library(beepr) #Beep when done to get my attention
beep()
svm.tune

p=predict(svm.tune, newdata=trainX[,1:nf])
ktrain=confusionMatrix(p, trainData$same)

p=predict(svm.tune, newdata=testData[,1:nf])
ktest=confusionMatrix(p, testData$same)

save(ktrain, ktest, file="DAT_report3.RDa")
