library(tseries)
library(forecast)
library(seasonal)
library(itsmr)

#checking datasets from R 
data()
nottem


plot(nottem)
acf(nottem)
pacf(nottem)

#spliting data into test - train data with 30-70 % ratio

n=length(nottem) # 240*.7=168
nottemtraining=ts(nottem[1:168],frequency = 12, start = c(1920,1))
nottemtest=ts(nottem[169:240],frequency = 12,start=c(1934,1))

adf.test(nottemtraining) # p-value = 0.01, alpha = 0.05 stationary
kpss.test(nottemtraining) # p-value =0.1, alpha = 0.05 stationary

tsdisplay(nottemtraining)

# d=0, D=0
# p=8, P=2
# q=2, Q=3

M1=Arima(nottemtraining,order = c(8,0,2),seasonal = c(2,0,3))
summary(M1) 
n=length(nottemtest)
fore_M1=forecast::forecast(M1,n)$mean
error_M1=nottemtest-fore_M1

MSE_M1=sum(error_M1**2)/n 
RMSE_M1=sqrt(MSE_M1)  
ME_M1=sum(error_M1)/n
MAE_M1=sum(abs(error_M1))/n 
PE_M1=100*(error_M1)/nottemtest
MPE_M1=sum(PE_M1)/n 
MAPE_M1=sum(abs(PE_M1))/n


M2=auto.arima(nottemtraining)
summary(M2) 

fore_M2=forecast::forecast(M2,n)$mean
error_M2=nottemtest-fore_M2


MSE_M2=sum(error_M2**2)/n 
RMSE_M2=sqrt(MSE_M2)  
MAE_M2=sum(abs(error_M2))/n 
PE_M2=100*(error_M2)/nottemtest
MPE_M2=sum(PE_M2)/n 
MAPE_M2=sum(abs(PE_M2))/n 

M3=ets(nottemtraining)
summary(M3)

fore_M3=forecast::forecast(M3,n)$mean
errorETS=nottemtest-fore_M3


MSE_M3=sum(errorETS**2)/n 
RMSE_M3=sqrt(MSE)  
ME_M3=sum(errorETS)/n 
MAE_M3=sum(abs(errorETS))/n 
PE_M3=100*(errorETS)/nottemtest
MPE_M3=sum(PE)/n 
MAPE_M3=sum(abs(PE))/n 

NT_Sed_cl = decompose(nottem)$seasonal
cladjusted=nottem- NT_Sed_cl

plot(cladjusted)
tsdisplay(cladjusted)

train_cl=ts(cladjusted[1:168],frequency = 12, start = c(1920,1))

adf.test(train_cl) #p-value=0.01 stationary
kpss.test(train_cl) #p-value=0.1  stationary



tsdisplay(train_cl)
M4=Arima(train_cl,order=c(1,0,1))
fore_M4=forecast::forecast(M4,h=n)$mean

forecl_M4=fore_M4 + cladjusted[169:240]

er_M4=nottemtest-forecl_M4


MSE_M4=sum(er_M4**2)/n 
RMSE_M4=sqrt(MSE_M4)  
ME_M4=sum(er_M4)/n
MAE_M4=sum(abs(er_M4))/n 
PE_M4=100*(er_M4)/nottemtest
MPE_M4=sum(PE_M4)/n 
MAPE_M4=sum(abs(PE_M4))/n 

### classical + ETS
M5 = ets(train_cl)
fore_M5=forecast::forecast(M5,h=n)$mean

forecl_M5=fore_M5 + cladjusted[169:240]
er_M5=nottemtest-forecl_M5


MSE_M5=sum(er_M5**2)/n 
RMSE_M5=sqrt(MSE_M5)  
ME_M5=sum(er_M5)/n
MAE_M5=sum(abs(er_M5))/n 
PE_M5=100*(er_M5)/nottemtest
MPE_M5=sum(PE_M5)/n 
MAPE_M5=sum(abs(PE_M5))/n 


forecast::forecast(M1,h=10)$mean
