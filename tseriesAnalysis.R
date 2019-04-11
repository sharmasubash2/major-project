# ARIMA Model to forecast no of outpatients for Galway University Hospital

# Load the combined data of in-patients, outpatients and day care
# Dataset is available on git link :https://github.com/sharmasubash2/major-project/blob/master/wait%20list%202014-2019_DR1.csv.zip
wait_list <- read.csv(file = "/Users/subashsharma/Documents/major-project/wait list 2014-2019_DR1.csv")
rawDataFrame <- as.data.frame(wait_list)
rawDataFrame$Archive.Date <- as.Date(rawDataFrame$Archive.Date, "%m/%d/%Y")
rawDataFrame$year <- format(as.Date(rawDataFrame$Archive.Date, format="%d/%m/%Y"),"%Y")
rawDataFrame$month <- format(as.Date(rawDataFrame$Archive.Date, format="%d/%m/%Y"),"%m")
galwayRawDf <- rawDataFrame[rawDataFrame$Hospital=="Galway University Hospital",]

library(dplyr)
library('forecast')
library('tseries')
library(scales)
library(ggplot2)

aggregateCountGalway=data.frame(aggregate(galwayRawDf$Count, by=list(galwayRawDf$year,galwayRawDf$month), FUN=sum))
names(aggregateCountGalway)<- c("year","month","NoofPatients")
aggregateCountGalway$date <- paste(aggregateCountGalway$year,"-", aggregateCountGalway$month,"-01", sep = "")
aggregateCountGalway$date = as.Date(aggregateCountGalway$date)
aggregateCountGalway <- subset(aggregateCountGalway, aggregateCountGalway$date<as.Date("2018-12-31"))

# Time Series from 2014 to 2018 for Number of Patients in Waiting List

ggplot(aggregateCountGalway, aes(aggregateCountGalway$date, aggregateCountGalway$NoofPatients)) + geom_line() + 
  scale_x_date('month')  + ylab("No of Patients in Waiting List Galway University Hospital") +
  xlab("Quarterly from 2014 to 2018") + 
  scale_x_date(date_breaks = "3 month", 
               labels=date_format("%b-%Y")
               )

# Convert No of Patients to a time series object

count_ts = ts(aggregateCountGalway[, c('NoofPatients')])

# Remove outliers

aggregateCountGalway$clean_cnt = tsclean(count_ts)

 # Plot after removing outliers


ggplot() +
  geom_line(data = aggregateCountGalway, aes(x = date, y = clean_cnt)) + ylab('No of Patients in Waiting List Galway University Hospital') + scale_x_date(date_breaks = "3 month", 
                                                                                                                               labels=date_format("%b-%Y")
  )

# Aggregate data quarterly
aggregateCountGalway$cnt_ma = ma(aggregateCountGalway$clean_cnt, order=3) # using the clean count with no outliers


# Moving Average
ggplot() +
  geom_line(data = aggregateCountGalway, aes(x = date, y = clean_cnt, colour = "Normal Plot")) +
  geom_line(data = aggregateCountGalway, aes(x = date, y = cnt_ma,   colour = "Quarterly Moving Average"))  +
  ylab('No of Patients in Waiting List Galway University Hospital') + scale_x_date(date_breaks = "3 month", 
                                        labels=date_format("%b-%Y")
  )

# Seasonality

count_ma = ts(na.omit(aggregateCountGalway$clean_cnt),start = c(2014,1), frequency=12)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# Stationery
adf.test(count_ma)

adf.test(count_ma, alternative = "stationary")

# Autocorrelations and Choosing Model Order

Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(4,1,0) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(1,1,24))
fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
fcast <- forecast(fit2, h=12)
plot(fcast) 

