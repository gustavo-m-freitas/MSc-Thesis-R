# -------------------------------------------------
#title: "Volatility Modeling"
#author: "Gustavo Freitas"
#subtitle: "Time Series"
# -------------------------------------------------

# -------------------------------------------------
# Load required packages
# -------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(slider)
library(ggplot2)
library(forecast)
library(e1071)
library(tseries)


# -------------------------------------------------
# 1. Load Processed Data
# -------------------------------------------------
ftse <- read_csv("rv_ftse_2025.csv", show_col_types = FALSE)

head(ftse, 5)

# -------------------------------------------------
# 2. Compute Realized Variance (RV)
# -------------------------------------------------
ftse_rv <- ftse %>% 
  mutate(rv = sqrt(rv))

# -------------------------------------------------
# 3- Realized Volatility Time Series Plot
# -------------------------------------------------
plot(ftse_rv$rv, type = "l", main = "Realized Volatility Time Series", ylab = "RV", xlab = "date")

# -------------------------------------------------
# 4- Realized Volatility Histogram and Density Plot
# -------------------------------------------------
ggplot(data = data.frame(rv = ftse_rv$rv), aes(x = rv)) +
  # Histogram with density scale
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +  
  # Add density line in red
  geom_density(color = "red", size = 1) +  
  # Title and axis labels
  labs(title = "Realized Volatility Histogram with Density", 
       x = "Realized Volatility", 
       y = "Density") +  
  # Classic theme for better visualization
  theme_classic()

# -------------------------------------------------
# 5- Normal Distribution Examples
# -------------------------------------------------
cat("P(X <= -1):", pnorm(-1), "\n")
cat("P(-1 < Z < 1):", pnorm(1) - pnorm(-1), "\n")
cat("P(-1.96 < Z < 1.96):", pnorm(1.959964) - pnorm(-1.959964), "\n")
cat("P(-2 < Z < 2):", pnorm(2) - pnorm(-2), "\n")
cat("P(-3 < Z < 3):", pnorm(3) - pnorm(-3), "\n")
cat("Z-value for 97.5% cumulative probability:", qnorm(0.975), "\n")

# -------------------------------------------------
# 6- Realized Volatility Boxplot
# -------------------------------------------------
boxplot(ftse_rv$rv, main = "Realized Volatility Boxplot", ylab = "RV")

# -------------------------------------------------
# 7- QQ-Plot to verify the normality of realized volatility
# -------------------------------------------------
qqnorm(ftse_rv$rv, main = "Realized Volatility QQ-Plot", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       ylim = c(-0.02, max(ftse_rv$rv))) 
qqline(ftse_rv$rv, col = "red", lwd = 2)

# -------------------------------------------------
# 8- Summary statistics
# -------------------------------------------------
summary(ftse_rv$rv)
print(paste("Var =", var(ftse_rv$rv, na.rm = TRUE)))

# -------------------------------------------------
# 9- Skewness and Kurtosis
# -------------------------------------------------
skewness(ftse_rv$rv, na.rm = TRUE)
kurtosis(ftse_rv$rv, na.rm = TRUE)

# -------------------------------------------------
# 10- Stationarity Tests (ADF and KPSS)
# -------------------------------------------------
adf_test_result <- adf.test(ftse_rv$rv, alternative = "stationary")
print(adf_test_result)

kpss_test_result <- kpss.test(ftse_rv$rv)
print(kpss_test_result)

# -------------------------------------------------
# 11- ACF and PACF plots
# -------------------------------------------------
acf(ftse_rv$rv, main = "ACF - Realized Volatility")
pacf(ftse_rv$rv, main = "PACF - Realized Volatility")

# -------------------------------------------------
# 12- Fit ARIMA model automatically
# -------------------------------------------------
auto_arima_model <- auto.arima(ftse_rv$rv)
summary(auto_arima_model)

# -------------------------------------------------
# 13 -p-value Calculation for ARIMA coefficients
# -------------------------------------------------
coef <- auto_arima_model$coef
se <- sqrt(diag(auto_arima_model$var.coef)) # Extract standard errors
t_values <- coef / se
p_values <- 2 * (1 - pnorm(abs(t_values)))

# -------------------------------------------------
# 14 Create results dataframe
# -------------------------------------------------
results_df <- data.frame(
  Coefficients = coef,
  Standard_Errors = se,
  T_Values = t_values,
  P_Values = p_values
)

print(results_df)

# -------------------------------------------------
# 15- Load AirPassengers dataset
# -------------------------------------------------
air <- read_csv("AirPassengers.csv", show_col_types = FALSE)
air <- air %>% rename(Passengers = `#Passengers`)

air$Month <- as.Date(air$Month, format = "%Y-%m")

# -------------------------------------------------
# 16- Train/Test Split and ARIMA Forecast for AirPassengers dataset
# -------------------------------------------------
air_ts <- ts(air$Passengers, start = c(1949, 1), frequency = 12)

train <- window(air_ts, end = c(1960, 7))
test <- window(air_ts, start = c(1960, 8))

# -------------------------------------------------
# 17- Fit ARIMA model on the training set
# -------------------------------------------------
arima_model <- auto.arima(train)

# -------------------------------------------------
# 18- Forecast for the test set period
# -------------------------------------------------
forecast_arima <- forecast(arima_model, h = length(test))

# -------------------------------------------------
# 19- Convert time series to data frames for ggplot
# -------------------------------------------------
train_df <- data.frame(
  Date = seq(as.Date("1949-01-01"), by = "month", length.out = length(train)),
  Passengers = as.numeric(train)
)

test_df <- data.frame(
  Date = seq(as.Date("1960-08-01"), by = "month", length.out = length(test)),
  Passengers = as.numeric(test)
)

forecast_df <- data.frame(
  Date = seq(as.Date("1960-08-01"), by = "month", length.out = length(test)),
  Prediction = as.numeric(forecast_arima$mean)
)

# -------------------------------------------------
# 20- Plot the results
# -------------------------------------------------
ggplot() +
  geom_line(data = train_df, aes(x = Date, y = Passengers, color = "Train")) +
  geom_line(data = test_df, aes(x = Date, y = Passengers, color = "Test")) +
  geom_line(data = forecast_df, aes(x = Date, y = Prediction, color = "Forecast")) +
  labs(title = "Train/Test Split with ARIMA Forecast",
       x = "Year-Month", y = "Passenger Number", color = "Legend") +
  theme_minimal()
