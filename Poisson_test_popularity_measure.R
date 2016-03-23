TwitterPoisson<-function(searchTerm1,searchterm2,maxTweets){
  tweetList1<-searchTwitter(searchTerm1,n=maxTweets)
  tweetList2<-searchTwitter(searchterm2,n=maxTweets)
  tweetdff1<-do.call("rbind",lapply(tweetList1,as.data.frame))
  tweetdff2<-do.call("rbind",lapply(tweetList2,as.data.frame))
  tweetO1<-tweetdff1[order(as.integer(tweetdff1$created)),]
  tweetO2<-tweetdff2[order(as.integer(tweetdff2$created)),]
  eventDelays1<-as.integer(diff(tweetO1$created))
  eventDelays2<-as.integer(diff(tweetO2$created))
  
  mean1<-mean(eventDelays1)
  mean2<-mean(eventDelays2)
  sum1<-sum(eventDelays1<=31)
  
  sum2<-sum(eventDelays2<=31)

 return(poisson.test(c(sum1,sum2),c(1000,1000)))
}