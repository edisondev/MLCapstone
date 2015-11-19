#Train Data via SVM
setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")

load(file="DAT_train_data_main.Rda")

library("e1071")
dfms$same=factor(dfms$same)

n_examples=8000
n_features=2000

hist_diff=as.data.frame(hist_diff)
hist_diff$same=dfms$same

hds=hist_diff[1:n_examples, c(1:n_features,2001)]
hdp=hist_diff[6000:6100, c(1:n_features,2001)]

start <- Sys.time ()
model <- svm(same~., data=hds,kernel="linear")
print(Sys.time () - start)

p=predict(model,newdata = hdp )

sum(p==hdp$same)/nrow(hdp)*100
table(pred=p, true=hdp$same)
