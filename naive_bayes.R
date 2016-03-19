sms_raw<-read.csv("sms_spam.csv",stringsAsFactors = FALSE)
sms_raw$type<-factor(sms_raw$type)
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
lapply(sms_corpus[1:3],as.character)
#data cleaning 
sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus_clean[[3]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean,replacePunctuation)
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)

class(sms_corpus_clean)
#data preparation
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm

#creating test and training dataset 
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
  return (x)
}
sms_train<-apply(sms_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN = 2,convert_counts)

sms_train1<-naiveBayes(sms_train,sms_train_labels)
sms_test_pred<-predict(sms_train1,sms_test)

CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
#imporoving the model
sms_train2<-naiveBayes(sms_train,sms_train_labels,laplace = 1)
sms_test_pred2<-predict(sms_train2,sms_test)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
              
