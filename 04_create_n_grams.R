setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")

#Load training Data
load(file="DAT_train_test_valid.Rda")
remove(dftest) #Do not touch test data


library(textcat) #library for N=grams

#make one long text from reviews
text=paste0(dftrain$review, sep=" ", collapse="") #Append multiple reviews


start <- Sys.time () #Check how long it takes
#Extract all 4-grams from text variable
profile=textcat_profile_db(text,
                     options=c("n"=4, #Use 4-grams 
                               "size"=100000,  #Up to 10,000 different n-grams
                               "reduce"=TRUE)) #Reduce multiple spaces to just one
print(Sys.time () - start)

#lapply(profile, head, 10L) #see top 10

table_feature_space <- as.matrix(profile) #convert the text to feature place
table_feature_space=table_feature_space[,1:1000] #Greater than 500
features_ordered=table_feature_space[order(names(table_feature_space))] #sort alphabetically

funi=names(features_ordered) #Sorted feature universe



#Append feature vector to each entry in data.frame of training data
load(file="DAT_train_data.Rda")

#Pick 10000 True values and 2000 FALSE values
nt=10000
nf=10000

indt=sample(c(1:50000), nt, replace=FALSE)
indf=sample(c(60000:72377), nf, replace=FALSE)
indc=c(indt,indf)
indc=indc[sample(c(1:(nt+nf)), (nt+nf), replace=FALSE)]
dfms=dfm[indc,]

hist_diff=matrix(data=0,nrow=nrow(dfms),ncol=length(funi), dimnames=list(1:nrow(dfms),funi)) #create empty hist

source("calc_histogram.R")

for (i in c(1:nrow(dfms)) ) {
  nhist1=n.gram.histogram(dfm$rev1[i],n=4,size=10000,funi)
  nhist2=n.gram.histogram(dfm$rev2[i],n=4,size=10000,funi)
  
  hist_diff[i,]=abs(nhist1/sum(nhist1) - nhist2/sum(nhist2))
  
  if(i%%1e2==0){ #Print progress and how long it takes per 1000 lines
    print(Sys.time () - start)
    print(i)
    start <- Sys.time ()
  }
  
}

save(dfms, funi, hist_diff, file="DAT_train_data_main.Rda")
# rev1=dfm$rev1[1]
# p1=textcat_profile_db(rev1,
#                            options=c("n"=4, #Use 4-grams 
#                                      "size"=5000,  #Up to 10,000 different n-grams
#                                      "reduce"=TRUE)) #Reduce multiple spaces to just one
# tfs1<- as.matrix(p1) #convert the text to feature place
# indfs = colnames(tfs1) %in% funi
# 
# hist1=matrix(data=0,nrow=1,ncol=length(funi), dimnames=list(1, funi))
# hist1[ 1, colnames(tfs1)[indfs] ] = tfs1[indfs] #add the profiles into the histogram
# 
# 
# 
# hist1=hist1/sum(hist1) #Normalize Profile


