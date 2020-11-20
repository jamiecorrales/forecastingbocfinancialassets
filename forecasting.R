
#Import Data 
Bocdata <- ts(bocdata, start = c(2010, 01), end = c(2020,08), frequency =12)

#Fill in NA values
df <- na.locf(bocdata)

#Subsetting Dataset 
financialassets.df <- df[, c(26)]

autoplot(financialssets.df)

#Forecasting with Naive and Snaive 

fafc <- naive(financialassets.df, h = 24)
sfafc <- snaive(financialassets.df, h = 24)
autoplot(fafc)
autoplot(sfafc)
summary(fafc)
summary(sfafc)

#Analyzing Residuals

checkresiduals(fafc)
checkresiduals(sfafc)

#Forecasting Method: Simple Exponential Smoothing 

fcfa.ses <- ses(financialassets.df, h = 24)
summary(fcfa.ses)
autoplot(fcfa.ses)
autoplot(fcfa.ses) + autolayer(fitted(fcfa.ses))

#Forecasting Method: Simple Exponential Smoothing 

fcfa.ses <- ses(financialassets.df, h = 24)
summary(fcfa.ses)
autoplot(fcfa.ses)
autoplot(fcfa.ses) + autolayer(fitted(fcfa.ses))



#Forecasting Method: Holt’s Winter multiplicative method 

fcfahw <- hw(financialassets.df, seasonal = "multiplicative", h = 24)
checkresiduals(fcfahw)summary(fcfahw)

autoplot(fcfahw)




#Forecasting Method: Holt’s Winter multiplicative method 

fcfahw <- hw(financialassets.df, seasonal = "multiplicative", h = 24)
checkresiduals(fcfahw)summary(fcfahw)

autoplot(fcfahw)

#Automative forecasting with exponential smoothins (ETS - errors, trend and seasonality)

fafitets <- ets(financialassets.df)
summary(fafitets)
checkresiduals(fafitets)
autoplot(forecast(fafitets))

#Forecasting Method: Automatic forecasting with ETS


ETS vs ARIMA using TSCV (Time-series cross validation) 
# Set up forecast functions for ETS and ARIMA models
ffaets <- function(x, h) {
  forecast(ets(x), h = h)
}
ffaarima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

# Compute CV errors for ETS on financialassets.df as e1
e1 <- tsCV(financialassets.df, ffaets, h = 1)

# Compute CV errors for ARIMA on financialassets.df as e2
e2 <- tsCV(financialassets.df, ffaarima, h = 1)

# Find MSE of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Plot 10-year forecasts using the best model class
financialassets.df %>% ffaarima(h = 24) %>% autoplot()
financialassets.df %>% ffaets(h = 24) %>% autoplot()


#another arima technique found on youtube https://www.youtube.com/watch?v=dBNy_A6Zpcc&ab_channel=AdamCheck

fit_arima <- auto.arima(financialassets.df, d=1, D=1, stepwise = FALSE, approximation=FALSE, trace=TRUE)
fit_arimafc <- forecast(fit_arima, h=24)
checkresiduals(fit_arima)
autoplot(fit_arimafc)


#Forecasting Method: TBATS Model 
tbatsfit <- tbats(financialassets.df)
tbatsfc <- forecast(tbatsfit, h = 24)
checkresiduals(tbatsfit)
autoplot(tbatsfc)






