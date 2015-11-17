#03_prep_training_data
#This file prepares the trianing data combinations

setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")

load(file="DAT_train_test_valid.Rda")
remove(dftest)

#Create a combination of all pairs that are EQUAL
library(plyr)
dft=ddply(dftrain, ~userid, summarize, t(combn(review,2)))

dft2=data.frame(rev1=dft$..1[1], 
                rev2=dft$..1[2], 
                same=as.logical(numeric(length = nrow(dft))+1),
                user1=dft$userid,
                user2=dft$userid,
                stringsAsFactors = FALSE)


#Create some combinations of reviews that are NOT EQUAL
entries=20000 #number of not equal combinations to create
dft3=data.frame(rev1=character(entries), #pre-allocate
                rev2=character(entries),
                same=logical(entries),
                user1=character(entries),
                user2=character(entries),
                stringsAsFactors = FALSE)
p=1
while (p < entries){
  k=sample(1:nrow(dftrain),2) #Randomly pick two users from dftrain
  if ( ! dftrain$userid[k[1]]==dftrain$userid[k[2]]){ #check so they are not the same
    dft3[p,]=list(dftrain$review[k[1]],
                  dftrain$review[k[2]],
                  same=F,
                  dftrain$userid[k[1]],
                  dftrain$userid[k[2]])
    p=p+1 #increment index
  }
}


#Save dft2 and dft3 to the save file
save(dft2,dft3, file="DAT_train_data.Rda")


#For testing of code
#k=data.frame(review=c(1,2,6,7,8),
#             userid=c("a","a","c","c","c"))
#
#l=ddply(k, ~userid, summarize, t(combn(review,2,simplify=T)))


