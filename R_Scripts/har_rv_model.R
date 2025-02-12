# ============================================================
# HAR-RV Model Implementation - MSc Thesis
# Author: Gustavo Freitas
# Master's Thesis: Forecasting FTSE-100 Volatility Using HAR-Type Models
# ============================================================

# --------------------------
# 1. Load Required Packages
# --------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(slider)
library(ggplot2)
library(lmtest)

# -----------------------------------
# 2. Load Processed FTSE-100 Dataset
# -----------------------------------
ftse <- read_csv("rv_ftse_2025.csv", show_col_types = FALSE)

# Check first observations
head(ftse, 5)

# -----------------------------------------
# 3. Compute Realized Volatility (RV)
# -----------------------------------------
ftse_rv <- ftse %>% 
  mutate(rv = sqrt(rv))  # Realized volatility as square root of variance

head(ftse_rv)

# -------------------------------------------------
# 4. Compute Weekly and Monthly Realized Volatility
# -------------------------------------------------
ftse_rv <- ftse_rv %>% 
  mutate(rv_lag_1 = lag(rv, 1),
         rv_weekly = lag(slide_dbl(rv, sum, .before = 4, .complete = TRUE), 1),
         rv_monthly = lag(slide_dbl(rv, sum, .before = 21, .complete = TRUE), 1)) %>% 
  drop_na()

head(ftse_rv)

# -------------------------------------------------
# 5. Estimate HAR Model Using Rolling Window
# -------------------------------------------------
forecast_har <- function(newdata){
  fit <- lm(rv ~ 1 + rv_lag_1 + rv_weekly + rv_monthly, data = newdata)
  predict(fit, newdata[NROW(newdata), , drop = FALSE])
}

har <- ftse_rv %>% 
  mutate(har = slide_dbl(.x = ., 
                         ~ forecast_har(.x), 
                         .before = 999, 
                         .complete = TRUE) %>% lag(1)) %>% 
  drop_na()

head(har)

# Applying lag(1) ensures that projections (har_s) are shifted forward by one step, 
# effectively excluding the first forecasted value. This adjustment prevents look-ahead bias 
# by ensuring that the forecast for time t is based solely on data available up to time t-1. 
# In financial models, this alignment maintains the integrity of predictive evaluation 
# by avoiding incorrect comparisons and ensuring that forecasts rely only on past information.

# ------------------------------------------------
# 6. Estimate HAR Model - In-sample analysis
# ------------------------------------------------
har_lm <- lm(rv ~ 1 + rv_lag_1 + rv_weekly + rv_monthly, data = ftse_rv)
summary(har_lm)

# -------------------------------------------------
# 7. Visualizing the HAR Model Forecast
# -------------------------------------------------
ggplot(har, aes(x = date)) +
  geom_line(aes(y = rv, color = "Realized Volatility (RV)"), linewidth = 1) +  
  geom_line(aes(y = har, color = "HAR Model Forecast"), linewidth = 1) +      
  labs(title = "Comparison of Realized Volatility and HAR Model Forecast",
       x = "Date", y = "Volatility",
       color = "Legend") + 
  theme_minimal() +
  theme(legend.position = "top")

# -----------------------------------
# 8. Loss Function Calculations
# -----------------------------------
error_metrics <- data.frame(Model = "HAR") %>% 
  mutate(har %>% 
           summarise(
             MAE = mean(abs(rv - har)),
             RMSE = sqrt(mean((rv - har)^2, na.rm = TRUE)),
             QLIKE = mean(log(har) + rv / har),
             R_2 = 1 - sum((rv - har)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE)
           ))

print(error_metrics)

# -----------------------------------
# 9. The Mincer–Zarnowitz Regression
# -----------------------------------
mz <- lm(rv ~ 1 + har, data = har)
fit_mz <- summary(mz)
print(fit_mz)

# -----------------------------------
# 10. Residual Analysis & Tests
# -----------------------------------
residuals <- har$rv - har$har

# Durbin-Watson Test (Autocorrelation)
dw_test <- dwtest(residuals ~ 1)
print(dw_test)

# Ljung-Box Test (Residual Independence)
lb_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
print(lb_test)

# Breusch-Pagan Test (Heteroskedasticity)
bp_test <- bptest(residuals ~ har$rv)
print(bp_test)

# Shapiro-Wilk Test (Normality of Residuals)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# -------------------------------------------------
# 11. Visualizing Residuals
# -------------------------------------------------

# 11.1 Residual Time Series Plot
ggplot(data = data.frame(index = 1:length(residuals), residual = residuals), aes(x = index, y = residual)) +
  geom_line(color = "blue") +
  theme_classic() +
  labs(title = "Residual Time Series", x = "Index", y = "Residual")

# 11.2 Histogram of Residuals
ggplot(data = data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_classic() +
  labs(title = "Residual Histogram with Density", x = "Residual", y = "Density")

# 11.3 Q-Q Plot of Residuals
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")

# 11.4 Residuals vs. Fitted Values Plot
ggplot(data = data.frame(fitted = har$rv, residuals = residuals), aes(x = fitted, y = residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_classic() +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# -------------------------------------------------
# 12. Autocorrelation Analysis
# -------------------------------------------------

# 12.1 RV Autocorrelation
bacf <- acf(har$rv, lag.max = 200, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, aes(x=lag, y=acf)) + 
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0, color = "blue") + 
  theme_classic() +  
  labs(title = "RV Autocorrelation", x = "Lag", y = "Autocorrelation")

# 12.2 HAR Autocorrelation
bacf <- acf(har$har, lag.max = 200, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, aes(x=lag, y=acf)) + 
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0, color = "blue") + 
  theme_classic() +  
  labs(title = "Autocorrelation Function - HAR-RV", x = "Lag", y = "Autocorrelation")




