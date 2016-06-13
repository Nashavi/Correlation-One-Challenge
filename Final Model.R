#### Load all required libraries ####
library(caret)
library(ggplot2)
library(pls)
library(DataCombine)

#### Load the csv data ####
rawdata<-read.csv(file.choose())
rawdata<-rawdata[1:100,]
head(rawdata)

#### Lag S2:S10 by -1, -2, -3 ####
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

#### Split train and test ####
train<-rawdata[1:50,]
test<-rawdata[51:100,]

### Create custom metric - Sum of Absolute Deviations ####
SAESummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- sum(abs(data$obs - data$pred), 
             na.rm = TRUE) 
  names(out) <- "SAE"
  out
}

#### Create Train Control ####
tc<- trainControl(summaryFunction = SAESummary)

#### Create TimeSlices with Fixed Window (For PLS)####
tsFixed <- createTimeSlices(1:nrow(train),initialWindow = 15, horizon = 1, fixedWindow = TRUE)
str(tsFixed,max.level = 1)
trainSlicesF <- tsFixed[[1]]
testSlicesF <- tsFixed[[2]]

#### Partial Least Squares Model train and predict ####
PLSpred<-NULL
true<-NULL
for(i in 1:length(trainSlicesF)){
  plsFit<- train(S1 ~  . -date,
                               data = train[trainSlicesF[[i]],],
                               method = "pls",
                               metric = "SAE",
                               preProc = c("center", "scale"),
                               maximize = FALSE,
                               trControl=tc)
  PLSpred[i]  <- predict(plsFit,train[testSlicesF[[i]],])
  true[i] <- train$S1[testSlicesF[[i]]]
}

#### Sum of Absolute Deviations for PLS model ####
PLS_SAE<-sum(abs(true-PLSpred))


#### PLSmodel plot vs true values ####
plsPlot<-plot(true, col = "red", ylab = "true values(red) , PLS preds (blue)", main = "Plot of S1 True Values Vs Predicted Values by PLS", ylim = range(c(PLSpred,true))); points(PLSpred, col = "blue") 

#### PLS Variable Importance measures ####
PLSvarimp<-varImp(plsFit)

####Create prediction file ####
submit<-data.frame(train$date[16:50],true,PLSpred)

#### Create TimeSlices with Non-Fixed Windows (For KNN & RF) ####
tsNonFixed <- createTimeSlices(1:nrow(train),initialWindow = 15, horizon = 1, fixedWindow = F)
str(tsNonFixed,max.level = 1)
trainSlicesNF <- tsNonFixed[[1]]
testSlicesNF <- tsNonFixed[[2]]

#### K-Nearest Neighbour Model train and predict ####
KNNpred<-NULL
true<-NULL
for(i in 1:length(trainSlicesNF)){
  knnFit <- train(S1 ~  . -date,
                               data = train[trainSlicesNF[[i]],],
                               method = "knn",
                               metric = "SAE",
                               maximize = FALSE,
                               preProc = c("center", "scale"),
                               trControl=tc)
  KNNpred[i]  <- predict(knnFit,train[testSlicesNF[[i]],])
  true[i] <- train$S1[testSlicesNF[[i]]]
}

#### Sum of Absolute Deviations for KNN model ####
KNN_SAE<-sum(abs(true-KNNpred))

#### KNN model plot vs true values ####
points(KNNpred, col = "green") 

#### KNN Variable Importance measures ####
KNNvarimp<-varImp(knnFit)

####Write to prediction file ####
submit$KNNpred<-KNNpred

#### Random Forest Tuning Parameters ####
RFtc <- trainControl(method = "repeatedcv",number = 10, repeats = 3,summaryFunction = SAESummary)
grid = expand.grid(mtry = c(6,8,10,12,14,15,16,18))

#### Random Forest Model train and predict ####
RFpred<-NULL
true<-NULL
for(i in 1:length(trainSlicesNF)){
  rfFit <- train(S1 ~  . -date,
                              data = train[trainSlicesNF[[i]],],
                              method = "rf",
                              metric = "SAE",
                              preProc = c("center", "scale"),
                              tuneGrid = grid,
                              maximize = FALSE,
                              trControl=RFtc)
  RFpred[i]  <- predict(rfFit,train[testSlicesNF[[i]],])
  true[i] <- train$S1[testSlicesNF[[i]]]
}

#### Sum of Absolute Deviations for RF model ####
RF_SAE<-sum(abs(true-RFpred))

#### RF model plot vs true values ####
points(RFpred, col = "yellow") 

#### RF Variable Importance measures ####
RFvarimp<-varImp(rfFit)

####Write to prediction file ####
submit$RFpred<-RFpred

head(submit)
submit$meanvalue<-(.30*submit$PLSpred+.10*submit$KNNpred+.60*submit$RFpred)
Overall_SAE<-sum(abs(submit$true-submit$meanvalue))

write.csv(submit,"submit.csv")


FinalPlot<-plot(submit$true, col = "red", ylab = "true values(red) , PLS preds (blue)", main = "Plot of S1 True Values Vs Predicted Values by final weighted model", ylim = range(c(submit$meanvalue,submit$true))); points(submit$meanvalue, col = "blue") 


predict()