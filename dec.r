library(C50)
library(e1071)
library(ggplot2)
library(dplyr)

dec<-function(){
data<-read.csv("C:/prgms/lenses.csv")
data$age<-as.factor(data$age)
data$prescription<-as.factor(data$prescription)
data$astigmatic<-as.factor(data$astigmatic)
data$tear_rate<-as.factor(data$tear_rate)
data$class<-as.factor(data$class)
model<-C5.0(class~.,data=data)
pre<-predict(model,newdata = data)
plot(model,main="dec tree",type="simple")
new<-data.frame(age<-readline(prompt = "age"),prescription<-readline(prompt = "pres"),
                astigmatic<-readline(prompt = "ast"),tear_rate<-readline(prompt = "tear"))
predicted<-predict(model,newdata = new)
cat(as.character(predicted))
}

svm<-function(){
data<-read.csv("C:/prgms/banknote.csv")
data$class<-as.factor(data$class)
model <- svm(class ~ variance + skewness, data = data, kernel = "radial")
predicted<-predict(model,newdata = data)
new<-data.frame(variance=as.integer(readline(prompt = "enter")),
                skewness=as.integer(readline(prompt = "enter")))
newp<-predict(model,newdata = new)
cat(newp)

}
menu<-function(){
  cat("1. 2. 3.")
  ch<-as.numeric(readline())
  if(ch==1){
    dec()
  }else if(ch==2){
    svm()
  }else{
    return()
  }
  menu()
}
menu()
