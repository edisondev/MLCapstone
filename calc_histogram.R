n.gram.histogram <- function(text,n=4,size=100000,histgram) {
  #This function calculates the n-gram histogram
  #Where:
  #   text    = text to analyze
  #   n       = the n in "n-grams" i.e. n=1,2,3,4
  #   size    = maximum number of grams
  #   histgram= previous hist feature space that this new one should fit in 
  #             (n-grams in text but no hist will be removed)
  
  profile=textcat_profile_db(text,
                             options=c("n"=n, #Use 4-grams 
                                       "size"=size,  #Up to 10,000 different n-grams
                                       "reduce"=TRUE)) #Reduce multiple spaces to just one
  
  tfs<- as.matrix(profile) #convert the text to feature place
  indfs = colnames(tfs) %in% histgram #find the overlap between the tfs and hist
  
  nhist=matrix(data=0,nrow=1,ncol=length(histgram), dimnames=list(1, histgram)) 
  nhist[ 1, colnames(tfs)[indfs] ] = tfs[indfs] #add the profiles into the histogram
  
  return(nhist)
}