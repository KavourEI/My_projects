library(geckor)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

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

ada_ts <- ts(data = ada_price$price, start = min(as.Date(ada_price$timestamp)), frequency = 365)

par(mfrow = c(1,1))
plot(ada_ts)

par(mfrow = c(1,2))
acf(ada_ts, main = "ACF plot of ADA")
pacf(ada_ts, main = "PACF plot of ADA")

l_ada=log(ada_ts)
dl_ada=diff(l_ada,1)

acf(ada_ts, main = "ACF plot of ADA")
pacf(ada_ts, main = "PACF plot of ADA")
acf(ts(l_ada), main = "ACF plot of lADA")
pacf(ts(l_ada), main = "PACF plot of lADA")
acf(ts(dl_ada), main = "ACF plot of dlADA")
pacf(ts(dl_ada), main = "PACF plot of dlADA")

Box.test(dl_ada, type = "Box-Pierce") # Good model fit

# ACF : t = 2,14,15,16,17,18,19,26
# PACF : t = 2,14,15,16,18,19,26

ar2fit=arima(dl_ada,order=c(2,0,0))
residuals_AR2 <- residuals(ts(ar2fit), frequency=365, start = min(as.Date(ada_price$timestamp)))


par(mfrow=c(1,2))        # set up the graphics  
acf(ts(residuals_AR2,freq=1), 48, main="ACF of residuals")        
pacf(ts(residuals_AR2,freq=1), 48, main="PACF of residuals") 
acf(ts(residuals_AR2^2,freq=1), 48, main="ACF of squared residuals")        
pacf(ts(residuals_AR2^2,freq=1), 48, main="PACF of squared residuals") 
qqnorm(residuals_AR2,main="Normal QQplot of residuals")  
qqline(residuals)  
