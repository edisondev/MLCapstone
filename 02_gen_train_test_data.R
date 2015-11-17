#File 02, Divides the data into training, validation, and testing data

setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")


#Load the reviews with greater than 500 words
load(file="review_data500.Rda")

#Remove users with only one 500 word review
df2=df[df$rev_count>1,]

num_reviews=sum(df$rev_count>1) #users with more than 2 reviews
users=unique(df2$userid)
num_users=length(users)

#Average large number of reviews per user:
num_reviews/num_users


#1000 users for training
#500 users for validation
#1455 users for final testing 

#In "Determining if two articles are by the same author" they use 1000 pairs.
#Given the average of 4.797 articles per user, this should give me 4000 pairs minimum

#Make Training Data (for Model Training)
ind_all=c(1:num_users) #indeces of all users
ind_train=sample(ind_all,1000,replace=FALSE)
dftrain=df2[df2$userid %in% users[ind_train],]

#Make Validation Data (for Model Selection)
ind_all=ind_all[-ind_train] #remove training indeces
ind_valid=sample(ind_all,500,replace=FALSE)
dfvalid=df2[df2$userid %in% users[ind_valid],]

#Make Testing Data (for final testing and reporting)
ind_all=c(1:num_users) 
ind_all=ind_all[-c(ind_valid,ind_train)] #remove validation and train indeces
dftest=df2[df2$userid %in% users[ind_all],]


save(dftest, dftrain, dfvalid, file="DAT_train_test_valid.Rda")


