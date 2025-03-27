library(nnet)
bino<-function(){
data<-read.csv("C:/prgms/logidata.csv")
model<-glm(Outcome~.,data = data,family = "binomial")
predicted<-predict(model,newdata = data,type="response")
new<-data.frame(Pregnancies=as.numeric(readline(prompt = "preg")),Glucose=as.numeric(readline(prompt = "glu")),
                BloodPressure=as.numeric(readline(prompt = "blood")),SkinThickness=as.numeric(readline(prompt = "skin")),
                Insulin=as.numeric(readline(prompt = "ins")),BMI=as.numeric(readline(prompt = "bmi")),
                DiabetesPedigreeFunction=as.numeric(readline(prompt = "diab")),Age=as.numeric(readline(prompt = "age")))
pre<-predict(model,newdata = new,type="response")
pred<-ifelse(pre>=0.5,1,0)
if(pred==1){
  cat("diab")
}else{
  cat("no")
}
acc<-mean(pred==data$Outcome)
print(acc)
}
multi<-function(){
data<-read.csv("C:/prgms/mushrooms.csv")
class<-as.factor(data$class)
capshape<-as.factor(data$capshape)
odor<-as.factor(data$odor)
habitat<-as.factor(data$habitat)
model <- multinom(class~capshape+odor+habitat, data = data)
predi<-predict(model,newdata = data)
new_data <- data.frame(
capshape=readline(prompt = "cap"),
odor=readline(prompt = "odor"),
habitat=readline(prompt = "hab")
)

prediction <- predict(model, newdata = new_data)
cat("\nPredicted class:", as.character(prediction), "\n")

# Calculate accuracy on the training data
accuracy <- mean(prediction == data$class)
cat('Accuracy for Multinomial Logistic Regression: ', round(accuracy, 4), "\n")

}
menu<-function(){
  cat("1. 2. 3.")
  ch<-as.numeric(readline())
  if(ch==1){
    bino()
  }else if(ch==2){
    multi()
  }else{
    return()
  }
  menu()
}
menu()
