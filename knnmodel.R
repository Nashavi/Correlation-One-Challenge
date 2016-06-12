library(caret)
library(ggplot2)
library(pls)
library(DataCombine)

rawdata<-read.csv(file.choose())
rawdata<-rawdata[1:100,]
head(rawdata)

lag<-1
rawdata<-slide(rawdata, Var = "S2", NewVar= "S2Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S3", NewVar= "S3Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S4", NewVar= "S4Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S5", NewVar= "S5Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S6", NewVar= "S6Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S7", NewVar= "S7Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S8", NewVar= "S8Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S9", NewVar= "S9Lag1",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S10", NewVar= "S10Lag1",slideBy = -lag)
lag<-2
rawdata<-slide(rawdata, Var = "S2", NewVar= "S2Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S3", NewVar= "S3Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S4", NewVar= "S4Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S5", NewVar= "S5Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S6", NewVar= "S6Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S7", NewVar= "S7Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S8", NewVar= "S8Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S9", NewVar= "S9Lag2",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S10", NewVar= "S10Lag2",slideBy = -lag)
lag<-3
rawdata<-slide(rawdata, Var = "S2", NewVar= "S2Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S3", NewVar= "S3Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S4", NewVar= "S4Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S5", NewVar= "S5Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S6", NewVar= "S6Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S7", NewVar= "S7Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S8", NewVar= "S8Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S9", NewVar= "S9Lag3",slideBy = -lag)
rawdata<-slide(rawdata, Var = "S10", NewVar= "S10Lag3",slideBy = -lag)

head(rawdata)

train<-rawdata[1:50,]
test<-rawdata[51:100,]

timeSlices <- createTimeSlices(1:nrow(train),initialWindow = 15, horizon = 1, fixedWindow = F)

str(timeSlices,max.level = 1)

trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]

head(train)
train[trainSlices[[1]],]

saeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- sum(abs(data$obs - data$pred), 
             na.rm = TRUE) 
  names(out) <- "SAE"
  out
}

tControl <- trainControl(summaryFunction = saeSummary)

pred<-NULL
true<-NULL
for(i in 1:length(trainSlices)){
  knnFitTimeLaggedSAE <- train(S1 ~  . -date,
                               data = train[trainSlices[[i]],],
                               method = "knn",
                               metric = "SAE",
                               maximize = FALSE,
                               preProc = c("center", "scale"),
                               trControl=tControl)
  pred[i]  <- predict(knnFitTimeLaggedSAE,train[testSlices[[i]],])
  
  true[i] <- train$S1[testSlices[[i]]]
  #   plot(true, col = "red", ylab = "true (red) , pred (blue)", 
  #        main = i, ylim = range(c(pred,true)))
  #   points(pred, col = "blue") 
}

sum(abs(true-pred)) #14.71826

knnPlot<-plot(true, col = "red", ylab = "true (red) , pred (blue)", main = "Plot of S1 True Values Vs Predicted Values by knn", ylim = range(c(pred,true))); points(pred, col = "blue") 

save(knnFitTimeLaggedSAE, file = "knnFitmodelLaggedSAE.Rdata")


varimplagged<-varImp(knnFitTimeLaggedSAE)
varimplagged

# plsFitTime
# ?train
# 
# submit<-read.csv(file.choose())
# submit$knnvalue<-pred
# 
# submit$mean<-(.50*submit$knnvalue+0.50*submit$pred)
# sum(abs(submit$true-submit$mean)) 
# plot(submit$true, col = "red", ylab = "true (red) , pred (blue)", main = "Plot of S1 True Values Vs Predicted Values by knn", ylim = range(c(submit$mean,submit$true))); points(submit$mean, col = "blue") 
#write.csv(submit,file="submit.csv")
