wbcd<-read_csv("/Users/adityabhat/Desktop/Machine Learning with R/chapter 3/wisc_bc_data.csv")
str(wbcd)
wbcd<-wbcd[-1]
wbcd$diagnosis<-factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
round(prop.table(table(wbcd$diagnosis))*100,digits=1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
wbcd_train<-wbcd_n[1:469,,drop=FALSE]
wbcd_test<-wbcd_n[470:569,,drop=FALSE]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_train_labels <-data.frame(wbcd_train_labels)
wbcd_test_labels <- as.factor(wbcd_test_labels[[1]])
wbcd_pred<-knn(train=wbcd_train, test=wbcd_test, cl=as.factor(wbcd_train_labels[[1]]),k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_pred,prop.chisq=FALSE)

wbcd_z<-as.data.frame(scale(wbcd[-1]))
