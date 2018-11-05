library(xts)
library(readr)
library(DMwR)
library(forecast)
library(tseries)

# Load chosen data
AirQualityUCI <- read_delim("AirQualityUCI.csv", 
                            ";", escape_double = FALSE, col_types = cols(`CO(GT)` = col_character(), 
                            X16 = col_skip(), X17 = col_skip(), T = col_character()),
                            trim_ws = TRUE,
                            na="-200")

# Join date and time to the same column
date_time <- paste(AirQualityUCI$Date, AirQualityUCI$Time)

# Remove previous columns
AirQualityUCI$Date = date_time
AirQualityUCI$Time = NULL

# Remove unecessary columns of the loaded dataset
AirQualityUCI$X16 = NULL
AirQualityUCI$X17 = NULL

AirQualityUCI$`NMHC(GT)` = NULL

# Process columns
# Convert strings to numeric when needed
AirQualityUCI$`CO(GT)` =  as.numeric(sub(",", ".", AirQualityUCI$`CO(GT)`, fixed = TRUE))
AirQualityUCI$`C6H6(GT)` =  as.numeric(sub(",", ".", AirQualityUCI$`C6H6(GT)`, fixed = TRUE))
AirQualityUCI$AH =  as.numeric(sub(",", ".", AirQualityUCI$AH, fixed = TRUE))
AirQualityUCI$T =  as.numeric(sub(",", ".", AirQualityUCI$T, fixed = TRUE))

# Format new date and type column
AirQualityUCI$Date = strptime(AirQualityUCI$Date, format="%d/%m/%Y %H.%M.%S")
summary(AirQualityUCI)
# Remove NA/missing values
AirQualityUCI = AirQualityUCI[!is.na(AirQualityUCI$Date),]
# Check class of the object containing dataset
class(AirQualityUCI)

# Remove unecessary columns of the loaded dataset (keep just Date and T columns)
df <- subset(AirQualityUCI, select = -c(2,3,4,5,6,7,8,9,10,12,13))
# Plot new data
plot(df)


df$T[is.na(df$T)] <- mean(df$T, na.rm = TRUE)

#### Using TS ####
time_series <- ts(df$T, frequency=24) # Frequency = 24 due to 24 observations per day (1 per hour)
plot.ts(time_series)
decomposed <- decompose(time_series)
plot(decomposed)
acf(time_series)

#### Check sTationarity ####
adf.test(time_series, alternative="stationary") # Not stationary

#### Remove Noise ####
season_adjusted = time_series - decomposed$seasonal
plot(season_adjusted)

adf.test(season_adjusted, alternative="stationary") #  Stationary
acf(season_adjusted)

#### Forecast using arima ####
fit_ar <- auto.arima(season_adjusted, seasonal=TRUE)

fit_arf <- forecast(fit_ar, h=24)
plot(fit_arf, include=50)

plotForecastErrors(fit_arf$residuals)

#### Predict using HoltWinters ####
(df.prediction <- HoltWinters(season_adjusted, gamma = FALSE))

plot(df.prediction)

#### Predict data ahead of 100 ####
(prediction <- predict(df.prediction, n.ahead=100, prediction.interval=TRUE)) 

plot.ts(season_adjusted, xlim=c(389,392), ylim=c(125,350))
lines(df.prediction$fitted[,1], col="green")
lines(prediction[,1], col = "blue") # Fit line for prediction
lines(prediction[,2], col = "red")  # Upper line for prediction
lines(prediction[,3], col = "red")  # Lower line for prediction

#### Predict using forecast ####
df.forecast <- forecast(df.prediction, h=24)
plot(df.forecast, include=50)


#### Error ####
df.forecast$residuals[is.na(df.forecast$residuals)] = 0 # If no difference set to 0
acf(df.forecast$residuals)

plot.ts(df.forecast$residuals)

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)x
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


plotForecastErrors(df.forecast$residuals)

