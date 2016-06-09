rawdata<-read.csv(file.choose())
train<-rawdata[1:50,]
test<-rawdata[51:100,]

k <- 5 # minimum data length for fitting a model
n <- length(train)
mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12)
st <- tsp(train[1,])[1]+(k-2)/1
train$date<-as.character(train$date)


corrdata<-cor(train[-1])
corrplot(corrdata, method = "circle")
corrdata
require(corrplot)
train$date<-as.Date(train$date)

as.date
str(train$date)

for(i in 1:(n-k))
{
  xshort <- window(a10, end=st + i/12)
  xnext <- window(a10, start=st + (i+1)/12, end=st + (i+12)/12)
  fit1 <- tslm(xshort ~ trend + season, lambda=0)
  fcast1 <- forecast(fit1, h=12)
  fit2 <- Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12), 
                include.drift=TRUE, lambda=0, method="ML")
  fcast2 <- forecast(fit2, h=12)
  fit3 <- ets(xshort,model="MMM",damped=TRUE)
  fcast3 <- forecast(fit3, h=12)
  mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
  mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}

