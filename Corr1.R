library(caret)
library(ggplot2)
library(pls)

rawdata<-read.csv(file.choose())
train<-rawdata[1:50,]
test<-rawdata[51:100,]

timeSlices <- createTimeSlices(1:nrow(train),initialWindow = 7, horizon = 1, fixedWindow = TRUE)

str(timeSlices,max.level = 1)

trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]

head(train)
train[trainSlices[[1]],]

plsFitTime <- train(S1 ~ S2+S3+S4+S5+S6+S7+S8+S9+S10,
                    data = train[trainSlices[[1]],],
                    method = "pls",
                    preProc = c("center", "scale"))

pred <- predict(plsFitTime,train[testSlices[[1]],])
pred
true <- train$S1[testSlices[[1]]]
true

plot(true, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred,true)))
points(pred, col = "blue") 

pred<-NULL
true<-NULL
for(i in 1:length(trainSlices)){
  plsFitTime <- train(S1 ~ S2+S3+S4+S5+S6+S7+S8+S9+S10,
                      data = train[trainSlices[[i]],],
                      method = "pls",
                      preProc = c("center", "scale"))
  pred[i]  <- predict(plsFitTime,train[testSlices[[i]],])
  
  
  true[i] <- train$S1[testSlices[[i]]]
  #   plot(true, col = "red", ylab = "true (red) , pred (blue)", 
  #        main = i, ylim = range(c(pred,true)))
  #   points(pred, col = "blue") 
}

sum(abs(true-pred)) #15.07404

plsPlot<-plot(true, col = "red", ylab = "true (red) , pred (blue)", main = "Plot of S1 True Values Vs Predicted Values", ylim = range(c(pred,true))); points(pred, col = "blue") 

save(plsFitTime, file = "plsFitmodel.Rdata")
