library(geckor)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(urca)
library(tseries)

ping()

current_market(coin_ids = c("cardano"), vs_currency = 'eur') %>% 
  glimpse()

cardano_history <-  coin_history(coin_id = "cardano",
                                 vs_currency = "eur",
                                 days = "max")
cardano_history %>% 
  ggplot(aes(timestamp,price))+
  geom_line() +
  theme_minimal()

ada_price <- cardano_history %>% 
  select(timestamp, price)

min(as.Date(ada_price$timestamp))
temp <- ada_price$timestamp
min(temp)

temp <- as.Date(ada_price$timestamp, format = "%Y %m %d")
ada_ts <- ts(data = ada_price$price, start = c(2017,290), frequency = 365)
ada_ts
summary(ur.df(ada_ts, type = 'trend')) # <--- Check how to interpret!
adf.test(ada_ts) # Not a stationary series.

par(mfrow = c(1,1))
plot(ada_ts)

par(mfrow = c(1,2))
acf(ada_ts, main = "ACF plot of ADA")
pacf(ada_ts, main = "PACF plot of ADA")

l_ada=log(ada_ts)
adf.test(l_ada)

dl_ada=diff(l_ada,1)
adf.test(dl_ada)

plot(dl_ada)

acf(ada_ts, main = "ACF plot of ADA")
pacf(ada_ts, main = "PACF plot of ADA")
acf(ts(l_ada), main = "ACF plot of lADA")
pacf(ts(l_ada), main = "PACF plot of lADA")
acf(ts(dl_ada), main = "ACF plot of dlADA")
pacf(ts(dl_ada), main = "PACF plot of dlADA")

Box.test(dl_ada, type = "Box-Pierce") # Good model fit

# ACF : t = 2,14,15,16,17,18,19,26
# PACF : t = 2,14,15,16,18,19,26

#AR2
ar2fit <- arima(dl_ada,order=c(2,0,0))
residuals_AR2 <- residuals(ts(ar2fit), frequency=365, start = min(as.Date(ada_price$timestamp)))

par(mfrow=c(1,2))        # set up the graphics  
acf(ts(residuals_AR2,freq= 1), 48, main="ACF of residuals")        
pacf(ts(residuals_AR2,freq=1), 48, main="PACF of residuals") 

acf(ts(residuals_AR2^2,freq=1), 48, main="ACF of squared residuals")
pacf(ts(residuals_AR2^2,freq=1), 48, main="PACF of squared residuals") 

qqnorm(residuals_AR2,main="Normal QQplot of residuals")  
qqline(residuals)  

# MA2 model construction
ma2fit <- arima(dl_ada, order = c(0,0,2))
residuals_MA2 <- residuals(ts(ma2fit), frequency = 365, start = min(as.Date(ada_price$timestamp)))

par(mfrow = c(2,2))
acf(ts(residuals_MA2, freq = 1), 48, main = "ACF of res(MA2)")
pacf(ts(residuals_MA2, freq = 1), 48, main = "PACF of res(MA2)")

# ARMA(2,2) model construction

arma22fit <- arima(dl_ada, order = c(2,0,2))
residuals_ARMA22 <- residuals(ts(arma22fit), frequency = 365, start = min(as.Date(ada_price$timestamp)))

acf(ts(residuals_ARMA22, freq = 1), 48, main = "ACF of res(ARMA22)")
pacf(ts(residuals_ARMA22, freq = 1), 48, main = "PACF or res(ARMA22)")

# AR(14) model construction

ar14fit <- arima(dl_ada, order = c(14,0,0))
residuals_AR14 <- residuals(ts(ar14fit), frequency = 365, start = min(as.Date(ada_price$timestamp)))

par(mfrow = c(1,2))
acf(ts(residuals_AR14, freq = 1), 48, main = "ACF of res(AR14)")
pacf(ts(residuals_AR14, freq = 1), 48, main = "PACF of res(AR14)")

# MA(14) model construction

ma14fit <- arima(dl_ada, order = c(0,0,14),)
residuals_MA14 <- residuals(ts(ma14fit), frequency = 365, start = min(as.Date(ada_price$timestamp)))

par(mfrow = c(1,2))
acf(ts(residuals_MA14, freq = 1), 48, main = "ACF of res(MA14)")
pacf(ts(residuals_MA14, freq = 1), 48, main = "PACF of res(MA14)")

# ARMA(14,14) model construciton

arma14fit <- arima(dl_ada, order = c(14,0,14))
residuals_ARMA14 <- residuals(ts(arma14fit), frequency = 365, start = min(as.Date(ada_price$timestamp)))

par(mfrow = c(1,2))
acf(ts(residuals_ARMA14, freq = 1), 48, main = "ACF of res(ARMA(14,14))")
pacf(ts(residuals_ARMA14, freq = 1), 48, main = "PACF of res(ARMA(14,14))")

future <- predict(arma14fit, 3)
future$pred

UL <- future$pred + future$se
LL <-  future$pred - future$se

par(mfrow = c(1,1))
ts.plot(dl_ada, future$pred)
lines(future$pred, col="red", type="o")
lines(UL, col="blue", lty="dashed") 
lines(LL, col="blue", lty="dashed")

