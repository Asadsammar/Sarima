# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(magrittr, pacman, psych, rio, tidyverse, readxl,dplyr,
               compareDF, ggplot2,grid, lubridate, stringr, openxlsx,forecast,TSA,FitAR)

Data <- read.delim2("./Data/CPI2010-24.txt")
View(Data)



par(mar=c(5,5,4,2))  # Adjust margins to make space for axis labels
years <- Data[,1]  # Extract the Year column
da <- ts(Data[,3])
da <- as.numeric(da)
plot(da, xlab="Year", ylab="CPI", col="blue", type="l", main="CPI Pakistan", xaxt="n", yaxt="n")
axis(1, at=1:length(da), labels=years)  # Use years for x-axis labels
axis(2)  # Add y-axis values
points(da, cex=0.5, col="red")
acf(da)
pacf(da)



#Identification of differencing Model 1
td1=diff(da)
plot.ts(td) 
acf(td1)
pacf(td1)

#Identification of differencing Model 2
td2=diff(diff(da))
plot.ts(td2) 
acf(td2,12) 
pacf(td2,12)
