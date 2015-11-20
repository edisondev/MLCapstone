#Train Data via SVM
setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")

load(file="DAT_train_data_main.Rda")

library("e1071")
dfms$same=factor(dfms$same)

n_examples=3000
n_features=1000

hist_diff=as.data.frame(hist_diff)
hist_diff$same=dfms$same

hds=hist_diff[1:n_examples, c(1:n_features,1001)]
hdp=hist_diff[6000:6100, c(1:n_features,1001)]

#Model#1
start <- Sys.time ()
model1 <- svm(same~., data=hds,kernel="linear")
print(Sys.time () - start)
p=predict(model1,newdata = hdp )
sum(p==hdp$same)/nrow(hdp)*100
res1=table(pred=p, true=hdp$same)
save(model1,res1, file="DAT_model1.Rda")

#Model#2
start <- Sys.time ()
model2 <- svm(same~., data=hds,kernel="linear", cost=0.001)
print(Sys.time () - start)
p=predict(model2,newdata = hdp )
sum(p==hdp$same)/nrow(hdp)*100
res2=table(pred=p, true=hdp$same)
save(model2,res2, file="DAT_model2.Rda")

#Model#3
start <- Sys.time ()
model3 <- svm(same~., data=hds,kernel="linear", cost=100)
print(Sys.time () - start)
t3=Sys.time () - start
p=predict(model3,newdata = hdp )
sum(p==hdp$same)/nrow(hdp)*100
res3=table(pred=p, true=hdp$same)
save(model3,res3, t3, file="DAT_model3.Rda")


#Model#4
start <- Sys.time ()
model4 <- svm(same~., data=hds)
print(Sys.time () - start)
t4=Sys.time () - start
p=predict(model4,newdata = hdp )
sum(p==hdp$same)/nrow(hdp)*100
res4=table(pred=p, true=hdp$same)
save(model4,res4,t4, file="DAT_model4.Rda")
