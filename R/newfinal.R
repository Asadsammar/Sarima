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

adf.test(difdt1)
ggtsdisplay(difdt1)

difdt2<- diff(diff(cpidt),lag=12)

adf.test(difdt2)
ggtsdisplay(difdt2)

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
