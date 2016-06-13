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
train<-rawdata[1:100,]


### Create custom metric - Sum of Absolute Deviations ####
SAESummary <- function (data,lev = NULL,model = NULL) {
  out <- sum(abs(data$obs - data$pred),na.rm = TRUE) 
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

#### Create TimeSlices with Non-Fixed Windows (For KNN & RF) ####
tsNonFixed <- createTimeSlices(1:nrow(train),initialWindow = 15, horizon = 1, fixedWindow = F)
str(tsNonFixed,max.level = 1)
trainSlicesNF <- tsNonFixed[[1]]
testSlicesNF <- tsNonFixed[[2]]

#### Random Forest Tuning Parameters ####
RFtc <- trainControl(method = "repeatedcv",number = 10, repeats = 3,summaryFunction = SAESummary)
grid = expand.grid(mtry = c(6,8,10,12,14,15,16,18))

#### Modelling and Prediction ####
PLSpred<-NULL
KNNpred<-NULL
RFpred<-NULL
true<-NULL
wghtdPred<-NULL
for(i in 1:length(trainSlicesF)){ #length of trainSlicesF and trainSlicesNF are the same
  
  #### Partial Least Squares Model train and predict ####
  plsFit<- train(S1 ~  . -date,
                 data = train[trainSlicesF[[i]],],
                 method = "pls",
                 metric = "SAE",
                 preProc = c("center", "scale"),
                 maximize = FALSE,
                 trControl=tc)
  PLSpred[i]  <- predict(plsFit,train[testSlicesF[[i]],])
  true[i] <- train$S1[testSlicesF[[i]]]
  
  
  #### K-Nearest Neighbour Model train and predict ####
  knnFit <- train(S1 ~  . -date,
                  data = train[trainSlicesNF[[i]],],
                  method = "knn",
                  metric = "SAE",
                  maximize = FALSE,
                  preProc = c("center", "scale"),
                  trControl=tc)
  KNNpred[i]  <- predict(knnFit,train[testSlicesNF[[i]],])
  true[i] <- train$S1[testSlicesNF[[i]]]
  
  #### Random Forest Model train and predict ####
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
  
  wghtdPred[i]<-(.30*PLSpred[i]+.10*KNNpred[i]+.60*RFpred[i])
  if (i<70) {
    train[trainSlicesF[[i+16]],]$S1[1]<-
                      ifelse(i+15>49, wghtdPred[i],train[trainSlicesF[[i+16]],]$S1[1])
    train[trainSlicesNF[[i+16]],]$S1[1]<-
                      ifelse(i+15>49, wghtdPred[i],train[trainSlicesNF[[i+16]],]$S1[1])
  } else {
    train[trainSlicesNF[[85]],]$S1[i+16]<-wghtdPred[i]
    }
}



#### Sum of Absolute Deviations for different models ####
PLS_SAE<-sum(abs(true-PLSpred))
KNN_SAE<-sum(abs(true-KNNpred))
RF_SAE<-sum(abs(true-RFpred))

#### Model plots vs true values ####
plsPlot<-plot(true, col = "red", pch=16, ylab = "true values(red) , PLS preds (blue)", main = "Plot of S1 True Values Vs Predicted Values by PLS", ylim = range(c(PLSpred,true))); points(PLSpred, col = "yellow"); points(KNNpred, col = "green"); points(wghtdPred, col = "blue",pch=16)

#### Variable Importance measures ####
#PLSvarimp<-varImp(plsFit)
#KNNvarimp<-varImp(knnFit)
#RFvarimp<-varImp(rfFit)

####Create prediction file ####
submit<-data.frame(train$date[16:100],true,PLSpred, KNNpred, RFpred, wghtdPred)
head(submit)
Overall_SAE<-sum(abs(submit$true-submit$wghtdPred))

write.csv(submit,"submit.csv")



