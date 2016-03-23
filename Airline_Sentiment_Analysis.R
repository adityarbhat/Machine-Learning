#Mining Twitter for Consumer attitudes for airlines
delta.tweets<- searchTwitter("@delta",n=150)
united.tweets<-searchTwitter("@UnitedAirlines",n=150)

tweets<-delta.tweets[[1]]
class(tweets)
tweets$getText()
united.text<-laply(united.tweets,function(u) u$getText())
delta.text<-laply(delta.tweets,function(t) t$getText())
head(delta.text,5)

hu.liu.pos<-scan("/Users/Aditya/Downloads/Positive:Negative txt/positive.txt", what="character",comment.char = ";")
hu.liu.neg<-scan("/Users/Aditya/Downloads/Positive:Negative txt/negative-words.txt", what="character",comment.char = ";")
pos.words<-c(hu.liu.pos,'upgrade')
neg.words<-c(hu.liu.neg,'wtf','wait','waiting','epicfail','mechanical')





score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence=iconv(sentence,"latin1","ASCII")
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  },pos.words, neg.words,.progress= .progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
delta.score<-score.sentiment(delta.text,pos.words,neg.words,.progress ='text')
united.score<-score.sentiment (united.text,pos.words,neg.words,.progress='text')
delta.score$airline='Delta'
delta.score$code='DL'
united.score$airline="United"
united.score$code="UA"

qplot(delta.score$score)
all.scores<-rbind(united.score,delta.score)
ggplot(data=all.scores)+
  geom_bar(mapping=aes(x=score,fill=airline),binwidth=1)+
  facet_grid(airline~.)+
  theme_bw()+scale_fill_brewer()
all.scores$very.neg<-as.numeric(all.scores$score>=-2)

twitter.df<-ddply(all.scores,c('airline','code'),summarise,pos.count=sum(very.pos),neg.count=sum(very.neg))
twitter.df$all.count<-twitter.df$pos.count+twitter.df$neg.count
twitter.df$score<-round(100*twitter.df$pos.count/twitter.df$all.count)
orderBy(~-score,twitter.df)

library(XML)
acsi.url<-'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'
acsi.df<-readHTMLTable(acsi.url,header = T, which = 1, stringAsFactor=F)
acsi.df<-acsi.df[,c(1,18)]
head(acsi.df,8)

colnames(acsi.df)<-c('airlines','score')
acsi.df$code<-c('DL','UA')
acsi.df$score<-as.numeric(acsi.df$score)

compare.df<-merge(twitter.df,acsi.df,by='code',suffixes =c('.twitter','.acsi'))
ggplot(compare.df)+
  geom_point(aes(x=score.twitter,y=score.acsi,color=airline.twitter),size=5)+
  geom_smooth(aes(x=score.twitter,
   y=score.acsi,group=1),se=F,method = "lm")+theme_bw()