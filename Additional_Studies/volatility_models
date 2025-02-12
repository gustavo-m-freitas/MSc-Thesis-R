# -----------------------------------------------------------
# Volatility Models - Time Series Analysis
# Author: Gustavo Freitas
# -----------------------------------------------------------

# -----------------------------------------------------------
# Load required packages
# -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(slider)
library(ggplot2)
library(forecast)
library(TTR)
library(fracdiff)
library(rugarch)
library(formattable)

# -----------------------------------------------------------
# 1. Load Processed Data
# -----------------------------------------------------------
ftse <- read_csv("ftse_rv_dwm2025.csv", show_col_types = FALSE)
head(ftse, 5)

# -----------------------------------------------------------
# 2. Estimate HAR model using a rolling moving window
# -----------------------------------------------------------
forecast_har <- function(newdata){
  fit <- lm(rv ~ 1 + rv_lag_1 + rv_weekly + rv_monthly, data = newdata)
  predict(fit, newdata[NROW(newdata), , drop = FALSE])
}

har <- ftse %>% 
  mutate(har = slide_dbl(.x = ., 
                         ~ forecast_har(.x), 
                         .before = 999, 
                         .complete = TRUE) %>% lag(1)) %>% 
  drop_na() %>% mutate(residuals = rv - har)

ggplot(har, aes(x = date)) +
  geom_line(aes(y = rv, color = "Realized Volatility (rv)"), linewidth = 1) +  
  geom_line(aes(y = har, color = "HAR Model Forecast"), linewidth = 1) +      
  labs(title = "Comparison of Realized Volatility and HAR Model Forecast",
       x = "Date", y = "Volatility",
       color = "Legend") + 
  theme_minimal() +
  theme(legend.position = "top")

# -----------------------------------------------------------
# 2.1 Calculate Loss Functions - MAE and RMSE
# -----------------------------------------------------------
loss_har <- har %>% summarise(
  RSS = sum((residuals)^2, na.rm = TRUE),
  MAE = mean(abs(residuals), na.rm = TRUE), 
  RMSE = sqrt(mean((residuals)^2, na.rm = TRUE)),
  R_2 = 1 - sum((residuals)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE)
)
print(loss_har)

# -----------------------------------------------------------
# 3. Creating Training and Testing Sets
# -----------------------------------------------------------
n <- nrow(ftse)
cutoff <- floor(0.8 * n)
train <- ftse[1:cutoff, ]
test <- ftse[(cutoff + 1):n, ]

cat("Training set:", nrow(train), "observations\n") 
cat("Testing set:", nrow(test), "observations\n") 

# -----------------------------------------------------------
# 4. Estimate Simple HAR Model
# -----------------------------------------------------------
fit_har_s <- lm(rv ~ 1 + rv_lag_1 + rv_weekly + rv_monthly, data = train)
test_har_s <- predict(fit_har_s, test)

har_s <- test %>% 
  mutate(har_s = lag(test_har_s, 1), residuals = rv - har_s) %>% 
  select(date, rv, har_s, residuals) %>% drop_na()

# -----------------------------------------------------------
# 5. ARIMA Model - Moving Window Forecasting**
# -----------------------------------------------------------

window <- 1000

# Function to fit ARIMA and forecast one step ahead
forecast_arima <- function(newdata) {
  model <- auto.arima(newdata)
  return(forecast(model, h = 1)$mean)
}

# Apply the function using a rolling window approach with slide_dbl
arima_forecast <- ftse %>%
  mutate(arima_pred = slide_dbl(.x = rv, 
                                .f = ~ forecast_arima(.x), 
                                .before = (window-1), 
                                .complete = TRUE) %>% lag(1)) %>% 
  drop_na() %>% mutate(residuals = rv - arima_pred)

# ARIMA Loss Functions
loss_arima <- arima_forecast %>% 
  summarise(RSS = sum((residuals)^2, na.rm = TRUE),
            MAE = mean(abs(residuals), na.rm = TRUE), 
            RMSE = sqrt(mean((residuals)^2, na.rm = TRUE)),
            R_2 = 1 - sum((residuals)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE))
print(loss_arima)

# Plot ARIMA Moving Window Forecasting
ggplot(arima_forecast, aes(x = date)) +
  geom_line(aes(y = rv, color = "Actual Volatility")) +
  geom_line(aes(y = arima_pred, color = "Rolling ARIMA Forecast")) +
  labs(title = "ARIMA Model with Rolling Window Forecasting",
       x = "Date", y = "Volatility",
       color = "Legend") +
  theme_minimal()


# -----------------------------------------------------------
# 6. Optimizing SMA and EMA Periods Based on MAE and RMSE
# -----------------------------------------------------------
# Define the length of the data and maximum window size to test
len <- length(ftse$rv)
max_period <- 100  # Test SMA and EMA for window sizes from 1 to 100

# Create a data frame to store MAE and RMSE for each period
results <- data.frame(Period = 1:max_period, SMA_MAE = NA, SMA_RMSE = NA, EMA_MAE = NA, EMA_RMSE = NA)

# Loop through each period to calculate SMA and EMA metrics
for (n in 1:max_period) {
  # Calculate SMA and EMA for the current period
  sma <- SMA(ftse$rv, n = n)  # Simple Moving Average
  ema <- EMA(ftse$rv, n = n)  # Exponential Moving Average
  
  # Calculate residuals with lag(1) to avoid look-ahead bias
  residual_sma <- ftse$rv - lag(sma, 1)
  residual_ema <- ftse$rv - lag(ema, 1)
  
  # Calculate Mean Absolute Error (MAE) and Root Mean Square Error (RMSE)
  results[n, "SMA_MAE"] <- mean(abs(residual_sma), na.rm = TRUE)
  results[n, "SMA_RMSE"] <- sqrt(mean(residual_sma^2, na.rm = TRUE))
  results[n, "EMA_MAE"] <- mean(abs(residual_ema), na.rm = TRUE)
  results[n, "EMA_RMSE"] <- sqrt(mean(residual_ema^2, na.rm = TRUE))
}

# Identify the best period (minimum MAE) for SMA and EMA
best_sma <- results[which.min(results$SMA_MAE), "Period"]
best_ema <- results[which.min(results$EMA_MAE), "Period"]

# Print the best periods for SMA and EMA
print(paste("Best SMA Period: ", best_sma, " periods"))
print(paste("Best EMA Period: ", best_ema, " periods"))

# -----------------------------------------------------------
# 6.1. Rolling oving window SMA - Simple Moving Average**
# -----------------------------------------------------------
# Function to calculate the best SMA in a rolling window of 1000 observations
calculate_best_sma <- function(data_window, max_period = 100) {
  errors <- sapply(1:max_period, function(n) {
    sma_values <- SMA(data_window, n = n)
    residuals <- data_window - lag(sma_values, 1)  # Avoid look-ahead bias
    mean(abs(residuals), na.rm = TRUE)  # Calculate MAE
  })
  which.min(errors)
}

# SMA rolling moving window
sma <- data.frame(rv = ftse$rv) %>%
  mutate(sma = slide_dbl(.x = seq_along(rv), 
                         ~ {
                           window <- rv[.x] 
                           if (length(window) < 1000) return(NA)  
                           best_period <- calculate_best_sma(window, max_period = 100)
                           sma_values <- SMA(window, n = best_period)
                           tail(sma_values, 1)  
                         },
                         .before = 999, 
                         .complete = TRUE) %>% lag(1)) %>%  drop_na() %>%
  mutate(residuals = rv - sma, obs = row_number())

# Calculate MAE, RMSE, and R^2 for SMA
sma_mae <- mean(abs(sma$residuals), na.rm = TRUE)
sma_rmse <- sqrt(mean(sma$residuals^2, na.rm = TRUE))
sma_r2 <- 1 - sum(sma$residuals^2, na.rm = TRUE) / sum((sma$rv - mean(sma$rv, na.rm = TRUE))^2, na.rm = TRUE)

# Plot SMA vs Actual Values
ggplot(sma, aes(x = obs)) +
  geom_line(aes(y = rv, color = "Actual Values"), size = 1) +
  geom_line(aes(y = sma, color = "SMA Forecast"), size = 1) +
  labs(title = paste("SMA vs Actual Volatility", sep = ""),
       x = "Observation",
       y = "Volatility") +
  theme_minimal()

# -----------------------------------------------------------
# 6.2. Best EMA - Exponential Moving Average**
# -----------------------------------------------------------
# Function to calculate the best EMA in a rolling window of 1000 observations
calculate_best_ema <- function(data_window, max_period = 100) {
  errors <- sapply(1:max_period, function(n) {
    ema_values <- EMA(data_window, n = n)
    residuals <- data_window - lag(ema_values, 1)  # Avoid look-ahead bias
    mean(abs(residuals), na.rm = TRUE)  # Calculate MAE
  })
  which.min(errors)
}

# EMA rolling moving window
ema <- data.frame(rv = ftse$rv) %>%
  mutate(ema = slide_dbl(.x = seq_along(rv), 
                         ~ {
                           window <- rv[.x] 
                           if (length(window) < 1000) return(NA)  
                           best_period <- calculate_best_ema(window, max_period = 100)
                           ema_values <- EMA(window, n = best_period)
                           tail(ema_values, 1)  
                         },
                         .before = 999, 
                         .complete = TRUE) %>% lag(1)) %>%  drop_na() %>%
  mutate(residuals = rv - ema, obs = row_number())

# Calculate MAE, RMSE, and R^2 for EMA
ema_mae <- mean(abs(ema$residuals), na.rm = TRUE)
ema_rmse <- sqrt(mean(ema$residuals^2, na.rm = TRUE))
ema_r2 <- 1 - sum(ema$residuals^2, na.rm = TRUE) / sum((ema$rv - mean(ema$rv, na.rm = TRUE))^2, na.rm = TRUE)

# Plot EMA vs Actual Values
ggplot(ema, aes(x = obs)) +
  geom_line(aes(y = rv, color = "Actual Values"), size = 1) +
  geom_line(aes(y = ema, color = "EMA Forecast"), size = 1) +
  labs(title = paste("EMA (", best_ema, "-Period) vs Actual Volatility", sep = ""),
       x = "Observation",
       y = "Volatility") +
  theme_minimal()

# -----------------------------------------------------------
## 6.3. Best EWMA - Exponentially Weighted Moving Average - lambda**
# -----------------------------------------------------------
# Define a range of ratio values to test
ratios <- seq(0.01, 0.99, by = 0.01)  # Test values from 0.01 to 0.99
mae_results <- numeric(length(ratios))  # Store MAE results

# Loop to test different ratios
for (i in seq_along(ratios)) {
  ewma_test <- EMA(ftse$rv, ratio = ratios[i])  # Compute EWMA for each ratio
  residuals <- ftse$rv - lag(ewma_test, 1)  # Align forecast with actual values
  mae_results[i] <- mean(abs(residuals), na.rm = TRUE)  # Compute MAE
}

# Find the best ratio (minimum MAE)
best_ratio <- ratios[which.min(mae_results)]
print(paste("Best lambda based on MAE:", best_ratio))

# Function to calculate the best EWMA ratio in a rolling window
calculate_best_ewma <- function(data_window, ratio_range = seq(0.01, 0.99, by = 0.01)) {
  errors <- sapply(ratio_range, function(ratio) {
    ewma_values <- EMA(data_window, ratio = ratio)  # Compute EWMA with the current ratio
    residuals <- data_window - lag(ewma_values, 1)  # Avoid look-ahead bias
    mean(abs(residuals), na.rm = TRUE)  # Calculate MAE
  })
  # Return the ratio that minimizes the MAE
  ratio_range[which.min(errors)]
}

# EWMA rolling moving window
ewma <- data.frame(rv = ftse$rv) %>%
  mutate(ewma = slide_dbl(.x = seq_along(rv), 
                          ~ {
                            window <- rv[.x]  
                            if (length(window) < 1000) return(NA) 
                            best_ratio <- calculate_best_ewma(window, ratio_range = seq(0.01, 0.99, by = 0.01))
                            ewma_values <- EMA(window, ratio = best_ratio)
                            tail(ewma_values, 1)  
                          },
                          .before = 999, 
                          .complete = TRUE) %>% lag(1)) %>%  
  drop_na() %>%   mutate(residuals = rv - ewma, obs = row_number())

# Calculate MAE, RMSE, and R^2 for EWMA
ewma_mae <- mean(abs(ewma$residuals), na.rm = TRUE)
ewma_rmse <- sqrt(mean(ewma$residuals^2, na.rm = TRUE))
ewma_r2 <- 1 - sum(ewma$residuals^2, na.rm = TRUE) / sum((ewma$rv - mean(ewma$rv, na.rm = TRUE))^2, na.rm = TRUE)

# Plot EWMA vs Actual Values
ggplot(ewma, aes(x = obs)) +
  geom_line(aes(y = rv, color = "Actual Values"), size = 1) +
  geom_line(aes(y = ewma, color = "EWMA Forecast"), size = 1) +
  labs(title = paste("EWMA with Optimized Ratio (", round(best_ratio, 3), ") vs Actual Volatility", sep = ""),
       x = "Observation",
       y = "Volatility") +
  theme_minimal()

# -----------------------------------------------------------
# 7. ARCH Model Selection
# -----------------------------------------------------------
# Define a function to find the best ARCH model
find_best_arch <- function(data, max_q) {
  results <- data.frame(Order = 1:max_q, AIC = NA, BIC = NA)
  
  for (q in 1:max_q) {
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q, 0)), 
                       mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
    fit <- ugarchfit(spec, data = data, solver = "hybrid")
    results[q, "AIC"] <- infocriteria(fit)[1]
    results[q, "BIC"] <- infocriteria(fit)[2]
  }
  
  return(results)
}

# Find the best ARCH model (max_q = 10)
max_q <- 10
arch_results <- find_best_arch(ftse$rv, max_q)

# Identify the best order based on AIC
best_arch_order <- which.min(arch_results$AIC)
print(paste("Best ARCH(q) model order based on AIC:", best_arch_order))
print(arch_results)

# Define the best ARCH model
best_q <- best_arch_order  # From previous step
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(best_q, 0)), 
                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))

# Fit the ARCH model
fit <- ugarchfit(spec, data = ftse$rv, solver = "hybrid")

# Extract fitted values (conditional variances)
fitted_volatility <- sigma(fit)

# Prepare data for plotting
arch_data <- data.frame(observation = 1:length(ftse$rv), 
                        rv = ftse$rv, 
                        fitted_volatility = lag(fitted_volatility,1)) %>% drop_na()

# Plot ARCH model vs Actual Values
ggplot(arch_data, aes(x = observation)) +
  geom_line(aes(y = rv, color = "Actual Volatility"), size = 1) +
  geom_line(aes(y = fitted_volatility, color = "ARCH Fitted Volatility"), size = 1) +
  labs(title = paste("ARCH(", best_q, ") Model vs Actual Volatility", sep = ""),
       x = "Observation",
       y = "Volatility") +
  theme_minimal()

# Calculate residuals for ARCH
arch_data <- arch_data %>%  mutate(residuals = rv - fitted_volatility)

# Calculate loss functions for ARCH
loss_arch <- arch_data %>%
  summarise(
    MAE = mean(abs(residuals), na.rm = TRUE),
    RMSE = sqrt(mean((residuals)^2, na.rm = TRUE)),
    R_2 = 1 - sum((residuals)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE)
  )

# Print the loss functions for ARCH
print(loss_arch)


# -----------------------------------------------------------
# 8. GARCH Model Selection
# -----------------------------------------------------------
# Define a function to find the best GARCH model
find_best_garch <- function(data, max_p, max_q) {
  results <- data.frame(P = integer(0), Q = integer(0), AIC = numeric(0), BIC = numeric(0))
  
  for (p in 1:max_p) {
    for (q in 1:max_q) {
      spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
      fit <- tryCatch(ugarchfit(spec, data = data, solver = "hybrid"), error = function(e) NULL)
      if (!is.null(fit)) {
        results <- rbind(results, data.frame(P = p, Q = q, 
                                             AIC = infocriteria(fit)[1], 
                                             BIC = infocriteria(fit)[2]))
      }
    }
  }
  return(results)
}

# Find the best GARCH model (max_p = 5, max_q = 5)
max_p <- 5
max_q <- 5
garch_results <- find_best_garch(ftse$rv, max_p, max_q)

# Identify the best order based on AIC
best_garch_order <- garch_results[which.min(garch_results$AIC), c("P", "Q")]
print(paste("Best GARCH(p, q) model order based on AIC: P =", best_garch_order$P, ", Q =", best_garch_order$Q))
print(garch_results)

# Define the best GARCH model
best_p <- best_garch_order$P  # From previous step
best_q <- best_garch_order$Q  # From previous step
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(best_p, best_q)), 
                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))

# Fit the GARCH model
fit <- ugarchfit(spec, data = ftse$rv, solver = "hybrid")

# Extract fitted values (conditional variances)
fitted_volatility <- sigma(fit)

# Prepare data for plotting
garch_data <- data.frame(observation = 1:length(ftse$rv), 
                         rv = ftse$rv, 
                         fitted_volatility = lag(fitted_volatility,1)) %>% drop_na()

# Plot GARCH model vs Actual Values
ggplot(garch_data, aes(x = observation)) +
  geom_line(aes(y = rv, color = "Actual Volatility"), size = 1) +
  geom_line(aes(y = fitted_volatility, color = "GARCH Fitted Volatility"), size = 1) +
  labs(title = paste("GARCH(", best_p, ",", best_q, ") Model vs Actual Volatility", sep = ""),
       x = "Observation",
       y = "Volatility") +
  theme_minimal()

# Calculate residuals for GARCH
garch_data <- garch_data %>% mutate(residuals = rv - fitted_volatility)

# Calculate loss functions for GARCH
loss_garch <- garch_data %>%
  summarise(
    MAE = mean(abs(residuals), na.rm = TRUE),
    RMSE = sqrt(mean((residuals)^2, na.rm = TRUE)),
    R_2 = 1 - sum((residuals)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE)
  )

# Print the loss functions for GARCH
print(loss_garch)

# -----------------------------------------------------------
# 9. Loss Functions Comparison Table
# -----------------------------------------------------------
# Create the data frame with models and metrics
error_metrics <- data.frame(
  Models = c("HAR", "ARIMA", "SMA", "EMA", "EWMA", "ARCH*", "GARCH*"),
  MAE = c(loss_har$MAE, loss_arima$MAE, sma_mae, ema_mae, ewma_mae, loss_arch$MAE, loss_garch$MAE),
  RMSE = c(loss_har$RMSE, loss_arima$RMSE, sma_rmse, ema_rmse, ewma_rmse, loss_arch$RMSE, loss_garch$RMSE),
  R_2 = c(loss_har$R_2, loss_arima$R_2, sma_r2, ema_r2, ewma_r2, loss_arch$R_2, loss_garch$R_2)
)

# Highlight the best values in the table
formatted_metrics <- formattable(
  error_metrics,
  list(
    MAE = formatter("span", style = x ~ style(color = ifelse(x == min(error_metrics$MAE), "green", "black"))),
    RMSE = formatter("span", style = x ~ style(color = ifelse(x == min(error_metrics$RMSE), "green", "black"))),
    R_2 = formatter("span", style = x ~ style(color = ifelse(x == max(error_metrics$R_2), "blue", "black")))
  )
)

# Print the formatted table
formatted_metrics

# Add explanation for the asterisk (*)
cat("\n* The parameters for 'ARCH*' and 'GARCH*' models were estimated using the entire dataset (in-sample), which can lead to overly optimistic results. In-sample evaluation means the model is trained and tested on the same data, making it prone to overfitting and using information from the future that would not be available in real-world forecasting.")

# -----------------------------------------------------------
# 10. Conclusion
# -----------------------------------------------------------
#This study aimed to explore various volatility models applied to the FTSE-100 index, 
# including HAR-type models, ARIMA, SMA, EMA, EWMA, and ARCH/GARCH. Each model was tested 
# to evaluate its ability to forecast realized volatility, with the results measured using 
# key performance metrics such as MAE, RMSE, and R².

# Among the models examined, the EMA (Exponential Moving Average) demonstrated the best 
# overall performance, achieving the lowest MAE and RMSE while attaining the highest R² 
# value. This highlights its strength as a forecasting tool for capturing the dynamics 
# of financial market volatility.

# It is essential to note that this work was conducted as a portfolio project to 
# demonstrate the application of volatility forecasting models and to showcase expertise 
#in their implementation. The results are not intended to serve as conclusive evidence
# that EMA is universally the best model for forecasting volatility. The analysis does 
# not account for statistical rigor often required in scientific research, and the models
# were not validated under diverse financial conditions or datasets.

# In summary, while the findings provide valuable insights into the comparative performance
# of various volatility models, this document should be interpreted as a demonstration of 
# applied modeling techniques rather than a definitive scientific study.
