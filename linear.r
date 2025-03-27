simple<-function(){
data<-read.csv("C:/prgms/linear.csv")
model<-lm(salary~years,data = data)
predicted<-predict(model,newdata=data)
print(predicted)
exp<-data.frame(years=as.numeric(readline(prompt = "enter")))
new<-predict(model,newdata=exp)
print(new)
me<-mean((data$salary-predicted)^2)
print(mse)
mae<-mean(abs(data$salary-predicted))
print(mae)
plot(data$years,data$salary)
abline(model,col="violet")
points(exp,new)
}s

multi<-function(){
data<-read.csv("C:/prgms/multilinear.csv")
ExtracurricularActivities<-as.factor(data$ExtracurricularActivities)
model<-lm(PerformanceIndex~HoursStudied+PreviousScores+ExtracurricularActivities+SleepHours+SamplePapersAttempted,data = data)
predicted<-predict(model,newdata = data)
new<-data.frame(HoursStudied=as.numeric(readline(prompt="study")), PreviousScores=as.numeric(readline(prompt="score")),
                ExtracurricularActivities=readline(prompt="extra"),SleepHours=as.numeric(readline(prompt="slepe")),
                SamplePapersAttempted=as.numeric(readline(prompt="attem")))
pred<-predict(model,newdata=new)
print(pred)
mse<-mean((data$PerformanceIndex-predicted)^2)
print(mse)
mae<-mean(abs(data$PerformanceIndex-predicted))
print(mae)
}
menu<-function(){
  cat("1. 2. 3.")
  ch<-as.numeric(readline())
  if(ch==1){
    simple()
  }else if(ch==2){
    multi()
  }else{
    return()
  }
  menu()
}
menu()
