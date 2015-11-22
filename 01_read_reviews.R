#File 01, reads the reviews with length >500 words

setwd("C:\\Users\\N\\Dropbox\\Coursera\\10 - Capstone Design Project\\yelp_dataset_challenge_academic_dataset")


##1st count the number of lines in the review data
##=======================================================

fil_con=file("yelp_academic_dataset_review.json") #establish file connection
open(fil_con)



k=length(readLines("yelp_academic_dataset_review.json")) #variable for counting


##2nd read the reviews and store reviews that are longer than 500 words
##=======================================================
library(jsonlite)
library(plyr)

entries=20000 #assume 20000 reviews with word count>500
#pre-allocate space 
df=data.frame( userid=character(entries), #user ID
               review=character(entries), #Review text
               stars=numeric(entries),    #Star rating
               wcount=numeric(entries),   #Word Count
               capitalization=numeric(entries),
               commas=numeric(entries),
               periods=numeric(entries),
               word_length_mean=numeric(entries), #average word length
               stringsAsFactors = FALSE)



fil_con=file("yelp_academic_dataset_review.json") #re-establish file connection
open(fil_con)

#read reviews and store them if word count is greater than 500
start <- Sys.time ()
p=1 #variable for indexing of the data frame
for(i in 1:k) {
  dat=readLines(fil_con,n=1) #read Lines
  
  temp=fromJSON(dat,simplifyVector=T)
  
  #Word Count Related Code
  review=temp$text
  str2 <- gsub(' {2,}',' ',review) #make sure only one space between words
  words=strsplit(str2,' ')[[1]] #split review into words
  wcount=length(words) #Get word count
  
  #Punctuation Parameters (based on 500 words only)
  review=paste(words[1:500], sep="", collapse=" ") #Only take the 1st 500 words of review
  text_len=nchar(review)
  
  capitals=sum( strsplit(review, split="")[[1]] %in% LETTERS ) #Count captials
  commas=text_len - nchar( gsub(",","" , review ) ) #Count commas
  periods=text_len - nchar( gsub("\\.","" , review ) )
  
  if (wcount>=500){
    df[p,]<-list(temp$user_id,
                 review,
                 temp$stars,
                 wcount,
                 capitals/text_len, #capitalization percentage
                 commas/text_len, #frequency of comma use
                 periods/text_len, #frequency of period use
                 mean(nchar(words)) #average word length
                 )
                 p=p+1
  }
  
  
  if(i%%1e4==0){ #Print progress and how long it takes per 1000 lines
    print(Sys.time () - start)
    print(i)
    start <- Sys.time ()
  }
}
close(fil_con)

## 3rd  count how many reviews each user has in the data frame
df=ddply(df,"userid", transform, rev_count=length(userid))

save(df, file="DAT_review_500.Rda")




