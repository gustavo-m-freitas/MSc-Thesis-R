# -------------------------------------------------
# LHAR-RV Model Implementation - Wen et al. (2016)
# Author: Gustavo Freitas
# Master's Thesis: Forecasting FTSE-100 Volatility Using HAR-Type Models
# -------------------------------------------------

# -------------------------------------------------
# Load required packages
# -------------------------------------------------
library(dplyr)
library(readr)
library(slider)
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggplot2)


# -------------------------------------------------
# 1. Load Processed Data
# -------------------------------------------------
ftse_5min <- read_csv("5min_ftse_2025.csv", show_col_types = FALSE)
head(ftse_5min, 5)

# -------------------------------------------------
# 2. Compute Realized Variance (RV)
# -------------------------------------------------
rv <- ftse_5min %>% 
  group_by(date) %>% 
  summarise(rv = sum(return^2)) %>% 
  mutate(rv = sqrt(rv))
head(rv)

# -------------------------------------------------
# 3. Compute Leverage (Negative Returns)
# -------------------------------------------------
r_neg <- ftse_5min %>% 
  group_by(date) %>% 
  summarise(r_neg = sum(return))

r_neg <- r_neg %>% 
  mutate(
    l_lag_1 = lag(ifelse(r_neg < 0, r_neg, 0), 1),
    l_weekly_lag_1 = lag(ifelse(slide_dbl(r_neg, mean, .before = 4, .complete = TRUE) < 0, 
                                slide_dbl(r_neg, mean, .before = 4, .complete = TRUE), 0), 1),
    l_monthly_lag_1 = lag(ifelse(slide_dbl(r_neg, mean, .before = 21, .complete = TRUE) < 0, 
                                 slide_dbl(r_neg, mean, .before = 21, .complete = TRUE), 0), 1)
  ) %>% drop_na() %>% select(-r_neg)

print(r_neg)

# -------------------------------------------------
# 4. Compute Weekly and Monthly Realized Volatility
# -------------------------------------------------
ftse_rv <- rv %>% 
  mutate(rv_lag_1 = lag(rv, 1),
         rv_weekly = lag(slide_dbl(rv, sum, .before = 4, .complete = TRUE),1),
         rv_monthly = lag(slide_dbl(rv, sum, .before = 21, .complete = TRUE),1)) %>% 
  drop_na()
head(ftse_rv)

# -------------------------------------------------
# 5. Merge Realized Volatility (ftse_rv) with Leverage Data (r_neg)
# -------------------------------------------------
ftse_rv <- ftse_rv %>% left_join(r_neg, by = "date")
head(ftse_rv)

# -------------------------------------------------
# 6. Estimate LHAR-RV Model Using a Rolling Moving Window
# -------------------------------------------------
forecast_har <- function(ftse_rv) {
  fit <- lm(rv ~ 1 + rv_lag_1 + rv_weekly + rv_monthly + 
              l_lag_1 + l_weekly_lag_1 + l_monthly_lag_1, data = ftse_rv)
  predict(fit, ftse_rv[NROW(ftse_rv), ])
}

lhar <- ftse_rv %>% 
  mutate(har = slide_dbl(.x = ., ~ forecast_har(.x), .before = 999, .complete = TRUE)) %>% 
  filter(!is.na(har))

head(lhar)

# -------------------------------------------------
# 7. In-sample analysis LHAR-RV Model 
# -------------------------------------------------
har_lm <- lm(rv ~ 1 + rv_lag_1 + rv_weekly + rv_monthly + 
               l_lag_1 + l_weekly_lag_1 + l_monthly_lag_1, data = ftse_rv)
summary(har_lm)

# -------------------------------------------------
# 8. Visualizing the LHAR-RV Model Forecast
# -------------------------------------------------
ggplot(lhar, aes(x = date)) +
  geom_line(aes(y = rv, color = "Realized Volatility (rv)"), linewidth = 1) +  
  geom_line(aes(y = har, color = "LHAR-RV Model Forecast"), linewidth = 1) +      
  labs(title = "Comparison of Realized Volatility and LHAR-RV Model Forecast",
       x = "Date", y = "Volatility",
       color = "Legend") + 
  theme_minimal() +
  theme(legend.position = "top")

# -------------------------------------------------
# 9. The Mincer–Zarnowitz Regression
# -------------------------------------------------
mz <- lm(rv ~ 1 + har, data = lhar)
fit_mz <- summary(mz)
print(fit_mz)

t_test <- (coefficients(fit_mz)[2] - 1)/fit_mz$coefficients[2, "Std. Error"]
p_value <- pt(abs(t_test), df = mz$df.residual, lower.tail = FALSE) * 2
p_value

# -------------------------------------------------
# 10. Loss Function Calculations
# -------------------------------------------------
har_rv_J_metrics <- read.csv("lossf_har_rv_j.csv")

lhar_rv_metrics <- data.frame(Model = "LHAR-RV") %>%  
  mutate(lhar %>% summarise(
    MAE = mean(abs(rv - har)),
    RMSE = sqrt(mean((rv - har)^2, na.rm = TRUE)),
    QLIKE = mean(log(har) + rv / har),
    R_2 = 1 - sum((rv - har)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE)
  ))

error_metrics <- rbind(har_rv_J_metrics, lhar_rv_metrics)
print(error_metrics)


# -------------------------------------------------
# 11. Residual Analysis
# -------------------------------------------------
residuals <- (lhar$rv - lhar$har)

# Durbin-Watson Test
dw_test <- dwtest(residuals ~ 1)
print(dw_test)

# Ljung-Box Test
lb_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
print(lb_test)

# Breusch-Pagan Test
bp_test <- bptest(residuals ~ lhar$rv)
print(bp_test)

# Shapiro-Wilk Test
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# -------------------------------------------------
# 12. Visualization of Residuals
# -------------------------------------------------
# Residual Time Series
ggplot(data = data.frame(index = 1:length(residuals), residual = residuals), aes(x = index, y = residuals)) +
  geom_line(color = "blue") +
  theme_classic() +
  labs(title = "Residual Time Series", x = "Index", y = "Residual")

# Histogram of Residuals
ggplot(data = data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_classic() +
  labs(title = "Residual Histogram with Density", x = "Residual", y = "Density")

# Q-Q Plot of Residuals
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")

# Residuals vs. Fitted Values
fitted_values <- lhar$har

ggplot(data = data.frame(fitted = fitted_values, residuals = residuals), aes(x = fitted, y = residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_classic() +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")


# -------------------------------------------------
# 13. Autocorrelation Analysis
# -------------------------------------------------
# 13.1 RV Autocorrelation
bacf <- acf(lhar$rv, lag.max = 200, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, aes(x=lag, y=acf)) + 
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0, color = "blue") + 
  theme_classic() +  
  labs(title = "RV Autocorrelation", x = "Lag", y = "Autocorrelation")

# 13.2 HAR Autocorrelation
bacf <- acf(lhar$har, lag.max = 200, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, aes(x=lag, y=acf)) + 
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0, color = "blue") + 
  theme_classic() +  
  labs(title = "Autocorrelation Function - HAR-RV", x = "Lag", y = "Autocorrelation")

# -------------------------------------------------
# Conclusion
# -------------------------------------------------
# The LHAR-RV model, incorporating leverage effects from negative returns, enhances 
# volatility forecasting accuracy compared to the standard HAR and HAR-RV-J models. 
# By integrating leverage terms, the model effectively captures asymmetric volatility 
# responses, particularly during periods of market downturns. The statistical analysis 
# confirms the significance of these components, with the weekly and monthly leverage 
# terms playing a crucial role in explaining realized volatility variations.

# Compared to previous models, LHAR-RV achieves the highest predictive accuracy, as 
# indicated by the lowest RMSE and MAE values and the highest R2. While residual 
# diagnostics highlight minor autocorrelation and heteroskedasticity issues, these 
# findings suggest potential refinements for improved robustness. Overall, the model 
# provides a strong framework for volatility forecasting, demonstrating the importance 
# of including leverage effects in financial econometric models.
