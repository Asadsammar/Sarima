# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(magrittr, pacman, psych, rio, tidyverse, readxl,dplyr,
               compareDF, ggplot2,grid, lubridate, stringr, openxlsx,
               forecast,TSA, tseries, MASS, scales, car,zoo,class,
               lmtest)
#load data from the disk
Data <- read.delim2("./Data/CPI2010-24.txt")
View(Data)


# extract CPI data only form dataset
dt  <- Data[, 3]
# convert Char data into numeric
dt <- as.numeric(dt)
# convert data into time series
cpidt <- ts(dt ,start = c(2010, 1), frequency = 12)

# plot the time series data
ggtsdisplay(cpidt)

adf.test(cpidt)

# cf(cpidt, main="CPI", )
# pacf(cpidt, main="CPI", lag.max =36)

#print summary of the data
sumr<-summary(cpidt)
print(sumr)
sd(cpidt)
var(cpidt)
skewness(cpidt)
kurtosis(cpidt)
sumr

#takw first difference to make data stationary 
difdt1 <- diff(cpidt,lag=12)

acf(difdt1, main="CPI",)
pacf(difdt1, main="CPI")


adf.test(difdt1)
ggtsdisplay(difdt1)

difdt2<- diff(diff(cpidt),lag=12)
acf(difdt2, main="CPI", lag.max =18)
pacf(difdt2, main="CPI ")

adf.test(difdt2)
ggtsdisplay(difdt2)

# BoxCox.lambda(cpidt)


# decomposed <- stl(ts(cpidt, frequency=12), s.window="periodic")
# plot(decomposed)

# autoplot(difdt1) + 
#   ggtitle("Seasonal plot of CPI")+
#   ylab("CPI")
# 
# ggseasonplot(difdt1)+ 
#   ggtitle("Seasonal plot of CPI")+
#   ylab("CPI")

#plot second difference
ggtsdisplay(difdt2)


# BoxCox.lambda(difdt2)

fit1 <- Arima(cpidt, order=c(0,1,3),
              seasonal=c(1,0,0), lambda=0,
              include.constant = TRUE)
#autoplot(fit1)
summary(fit1)

coeftest(fit1)
checkresiduals(fit1)
ggtsdisplay(fit1$residuals)


fit2 <- Arima(cpidt, order=c(0,1,0),
              seasonal=c(0,1,1), lambda=0,
              include.constant = TRUE)
autoplot(fit2)
coeftest(fit2)
checkresiduals(fit2)
ggtsdisplay(fit2$residuals)



fit3 <- Arima(cpidt, order=c(0,2,1),
              seasonal=c(0,0,2), lambda=0,
              include.constant = TRUE)
#autoplot(fit3)
coeftest(fit3)
summary(fit3)
checkresiduals(fit3)
ggtsdisplay(fit3$residuals)
accuracy(fit3)




fit4 <- Arima(cpidt, order=c(1,1,0),
              seasonal=c(1,0,0), lambda=0,
              include.constant = TRUE)
#autoplot(fit3)
coeftest(fit4)
summary(fit4)
checkresiduals(fit4)
ggtsdisplay(fit4$residuals)

forc = forecast(fit3 , h = 24)
plot(forc)




qqPlot(fit3$residuals, main = "Normal Q-Q Plot of CPI", envelope = 0.95)





Arimafit <- auto.arima(cpidt,
                       approximation = FALSE,
                       trace = TRUE,
                       ic = "aic",
                       test = "kps")



summary(Arimafit)
arimaorder(Arimafit)
forcast <- forecast(Arimafit, h=24)
plot(forcast, main = "Forecast of CPI", xlab = "Year", ylab = "CPI", col = "red", lwd = 2)
points(time(forcast), forcast, col = "blue", pch = 16)

res <- residuals(Arimafit)
acf(res)
pacf(res)

autoplot(cpidt, series = "Oserved", size = 1.2)+
  autolayer(Arimafit$fitted, series = "Fitted", size = 1.2)

autoplot(cpidt, series = "Original", size = 1.2)+
  autolayer(fit2$fitted, series = "Fitted line", size = 1.2)




#  #Q-Q norm plots
# qqnorm(difdt2)
# qqline(difdt2)
# qqnorm(difdt2, main = "Normal Q-Q Plot of CPI", pch = 19, col = "steelblue")
# qqPlot(difdt2, main = "Normal Q-Q Plot of CPI", envelope = 0.95)
# qqnorm(difdt2, main = "Normal Q-Q Plot of CPI", xlab = "Normal Quantiles", ylab = "CPI Ordered Data")
# qqPlot(difdt2, main = "Normal Q-Q Plot of CPI", envelope = 0.95)
# 

ggtsdisplay(da)
adf.test(da)
dif1<- diff(da)
adf.test(dif1)
dif2 <- diff(dif1)
dif3 <- diff(dif2)
ggtsdisplay(dif1)
ggtsdisplay(dif2)
ggtsdisplay(dif3)

 

BoxCox.lambda(da)
transformed_data <- (da^(-0.879) - 1) / (-0.879)
ggtsdisplay(transformed_data)
diff1trans <- diff(transformed_data)
ggtsdisplay(diff1trans)
diff2trans <- diff(diff1trans)
ggtsdisplay(diff2trans)
BoxCox.lambda(transformed_data)


model <- auto.arima(cpidt, seasonal = TRUE, lambda = 1)
summary(model)
forcast = forecast(model, h = 12)
plot(forcast)
