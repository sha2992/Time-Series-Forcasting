#required library  
library(tseries)
library(forecast)
library(seasonal)
library(itsmr)

#checking datasets from R 
data()

#AirPassengers data
AirPassengers

#plotting data for checking components 
plot(AirPassengers)
acf(AirPassengers)
pacf(AirPassengers)

#spliting data into test - train data with 30-70 % ratio

length(AirPassengers) # 144*.7=100.8

trainingData=ts(AirPassengers[1:101],frequency = 12, start = c(1949,1))
testData=ts(AirPassengers[102:144],frequency = 12,start=c(1957,6))

#stationary test
adf.test(trainingData) # p-value = 0.01, alpha = 0.05 stationary
kpss.test(trainingData) # p-value =0.01, alpha = 0.05 non-stationary

#L=BoxCox.lambda(trainingData)
#transformed_trainingData = BoxCox(trainingData,lambda = L)

adf.test(trainingData)
#Seasonal difference
DiffSTrainData = diff(trainingData,lag=1)

#Non-Seasonal difference
DiffTrainData=diff(DiffSTrainData,difference=1)

adf.test(DiffTrainData) # p-value = 0.01, alpha = 0.05 stationary
kpss.test(DiffTrainData) # p-value = 0.1, alpha = 0.05 stationary

tsdisplay(DiffTrainData)
acf(DiffTrainData)
pacf(DiffTrainData)
# d=1, D=1
# p=2, P=0
# q=2, Q=2

M1=Arima(trainingData,order = c(2,1,2),seasonal = c(0,1,2))
summary(M1) # AIC = 653.75, BIC=671.03, AICc=655.15

predict(M1,n.ahead=10)

forecast::forecast(M1, h = 10)$mean
n=length(testData)
fore_M1=forecast::forecast(M1,n)$mean
error_M1=testData-fore_M1

MSE_M1=sum(error_M1**2)/n ##698.2253
RMSE_M1=sqrt(MSE_M1)  ##26.42395
ME_M1=sum(error_M1)/n
MAE_M1=sum(abs(error_M1))/n ##22.65318
PE_M1=100*(error_M1)/testData
MPE_M1=sum(PE_M1)/n ## -2.853441
MAPE_M1=sum(abs(PE_M1))/n ## 5.372864


M2=auto.arima(trainingData)
summary(M2) # AIC = 650.55, BIC=657.99, AICc=650.84

fore_M2=forecast::forecast(M2,length(testData))$mean
error_M2=testData-fore_M2

n=length(testData)
MSE_M2=sum(error_M2**2)/n ##735.5534
RMSE_M2=sqrt(MSE_M2)  ##27.12109
ME_M2=sum(error_M2)/n
MAE_M2=sum(abs(error_M2))/n ##22.65318
PE_M2=100*(error_M2)/testData
MPE_M2=sum(PE_M2)/n ## -3.090972
MAPE_M2=sum(abs(PE_M2))/n ## 5.549548

M3=ets(trainingData)
summary(M3)

fore_M3=forecast(M3,length(testData))$mean
errorETS=testData-fore_M3

n=length(testData)
MSE=sum(errorETS**2)/n ##1929.402
RMSE=sqrt(MSE)  ##43.92496
ME=sum(errorETS)/n ## 25.97119
MAE=sum(abs(errorETS))/n ##33.44771
PE=100*(errorETS)/testData
MPE=sum(PE)/n ##5.13516
MAPE=sum(abs(PE))/n ## 7.262512


AP_Sed_X11 = seas(AirPassengers,x11="") 
SX11_AirPass = seasonal(AP_Sed_X11)
SX11_Adj_AirPass=seasadj(AP_Sed_X11)
plot(SX11_Adj_AirPass)
tsdisplay(SX11_Adj_AirPass)

PredictModel = Arima(SX11_Adj_AirPass,order = c(2,1,2))

forecast::forecast(PredictModel,h=10)$mean

AirPassengers
train_X11=seas(trainingData,x11="")
train_Adj_X11=seasadj(train_X11)

adf.test(train_Adj_X11) #p-value=0.7933 non-stationary
kpss.test(train_Adj_X11) #p-value=0.01  non-stationary

Diff_train_Adj_X11=diff(train_Adj_X11,differences = 1)
adf.test(Diff_train_Adj_X11) #p-value = 0.01 null hypothesis rejected, stationary
kpss.test(Diff_train_Adj_X11) #p-value = 0.08665 null hypothesis accepted, stationary

ggsubseriesplot(Diff_train_Adj_X11)
tsdisplay(Diff_train_Adj_X11)
M4=Arima(train_Adj_X11,order=c(1,1,1))
fore_M4=forecast(M4,h=n)$mean

foreSE_M4=fore_M4 + SX11_AirPass[102:144]

er_M4=testData-foreSE_M4

#n=length(testData)
MSE_M4=sum(er_M4**2)/n ##9311.23
RMSE_M4=sqrt(MSE_M4)  ##96.4947
ME_M4=sum(er_M4)/n ## 58.7751
MAE_M4=sum(abs(er_M4))/n ## 72.8657
PE_M4=100*(er_M4)/testData
MPE_M4=sum(PE_M4)/n ## 11.209322
MAPE_M4=sum(abs(PE_M4))/n ## 15.546569

test(M4$residuals)

### X-11 + ETS
M5 = ets(train_Adj_X11)
fore_M5=forecast::forecast(M5,h=n)$mean

foreSE_M5=fore_M5 + SX11_AirPass[102:144]

er_M5=testData-foreSE_M5

#n=length(testData)
MSE_M5=sum(er_M5**2)/n ## 4643.343
RMSE_M5=sqrt(MSE_M5)  ## 68.14208
ME_M5=sum(er_M5)/n
MAE_M5=sum(abs(er_M5))/n ## 55.73959
PE_M5=100*(er_M5)/testData
MPE_M5=sum(PE_M5)/n ## -0.8791963
MAPE_M5=sum(abs(PE_M5))/n ## 12.99315


forecast(M1,levels=c(95),h=10)$mean
#forecast = 
#predict(M1, n.ahead=10)

#acf(train_Adj_X11)
#pacf(train_Adj_X11)
# Checking Stationary or non-stationary
adf.test(AirPassengers) # p-value = 0.01, alpha = 0.05 non-stationary 

kpss.test(AirPassengers) # p-value =0.01

tsdisplay(AirPassengers)

DSAp=diff(AirPassengers, lag=1)
DAp=diff(DSAp,differences = 1)
adf.test(DAp)
tsdisplay(DAp)

#DSAap = diff(DAp, lag = 1)
#DAap=diff(DSAap,differences=1)
#tsdisplay(DAap)

#d=1, D=1 

#p=4, P=1

#q=4, Q=3

M1= Arima(DAp,order = c(4,1,4),seasonal = c(1,1,3))
summary(M1) # AIC = 1034.25, BIC=1071.43, AICc=1037.42

#L=BoxCox.lambda(AirPassengers) # -0.29471558556
#AirPass = BoxCox(AirPassengers,lambda = L)

#adf.test(AirPass)
#tsdisplay(AirPass)

auto.arima(DAp)

#auto.arima(JohnsonJohnson)
