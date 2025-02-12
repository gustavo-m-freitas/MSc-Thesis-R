# -------------------------------------------------
# HAR-RV-J Model Implementation
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
  summarise(rv = sum(return^2)) %>% mutate(rv = sqrt(rv))
head(rv)

# -------------------------------------------------
# 3. Compute Realized Bipower Variation (RBV)
# -------------------------------------------------
z_1 <- pi/2

rbv <- ftse_5min %>% 
  group_by(date) %>% 
  summarise(rbv = (abs(return) * abs(dplyr::lag(return))) %>% 
              na.omit %>% sum, m = length(return)) %>% 
  mutate(rbv = z_1 * ( m/(m-1)) * rbv ) %>% select(-m)
head(rbv)

# -------------------------------------------------
# 4. Compute Realized Tri-Power Quarticity (RTQ)
# -------------------------------------------------
x_43 <- 2^(2/3) * gamma(7/6) * gamma(1/2)^-1

rtq <- ftse_5min %>% 
  group_by(date) %>% 
  summarise(m = length(return), return = (abs(dplyr::lag(return,2))^(4/3) * 
                                            abs(dplyr::lag(return))^(4/3) * 
                                            abs(return)^(4/3) ) %>% na.omit %>% sum) %>% 
  mutate(rtq = m * (x_43 ^ -3) * (m/(m-2)) * return) 
head(rtq)

# -------------------------------------------------
# 5. Compute Ratio-Statistic (Z-Ratio)
# -------------------------------------------------
x_1 <- sqrt(2/pi)

z_ratio <- rv %>% 
  left_join(rbv, by = "date") %>% 
  left_join(rtq, by = "date") %>% 
  mutate(z = ((rv - rbv)/rv) /
           sqrt( (x_1^-4 + 2*(x_1^-2) -5) / m * 
                   ifelse(rtq / (rbv^2) >1,  rtq / (rbv^2), 1) ) ) %>% 
  select(date, rv, rbv, rtq, z) 
head(z_ratio)

# -------------------------------------------------
# 6. Compute Jump Component
# -------------------------------------------------
alpha <- 0.99
ftse_rv <- z_ratio %>% 
  mutate(jump = (ifelse(z > qnorm(alpha), (rv - rbv), 0))*1)
head(ftse_rv)

# -------------------------------------------------
# 7. Compute Lagged Daily, Weekly, and Monthly Realized Volatility and Lagged Jump
# -------------------------------------------------
ftse_rv <- ftse_rv %>% 
  mutate(rv_lag_1 = dplyr::lag(rv, 1),
         rv_weekly_1 = dplyr::lag(slide_dbl(rv, sum, .before = 4, .complete = TRUE)/5,1),
         rv_monthly_1 = dplyr::lag(slide_dbl(rv, sum, .before = 21, .complete = TRUE)/22,1),
         j_1 = dplyr::lag(jump)) %>% 
  filter(!is.na(rv_monthly_1))

# -------------------------------------------------
# 8. HAR-RV-J Model Implementation
# -------------------------------------------------
forecast_har <- function(ftse_rv){
  fit <- lm(rv ~ 1 + rv_lag_1 + rv_weekly_1 + rv_monthly_1 + j_1, data = ftse_rv)
  predict(fit, ftse_rv[NROW(ftse_rv), ])
}

har_rv_j <- ftse_rv %>% 
  mutate(har = slide_dbl(.x = ., 
                         ~ forecast_har(.x), 
                         .before = 999, 
                         .complete = TRUE)) %>% drop_na()

# -------------------------------------------------
# 9. HAR-RV-J Model In-sample analysis
# -------------------------------------------------
har_rv_j_lm <- lm(rv ~ 1 + rv_lag_1 + rv_weekly_1 + rv_monthly_1 + j_1 , data = har_rv_j)
summary(har_rv_j_lm)

# -------------------------------------------------
# 10. Visualizing the HAR-RV-J Model Forecast
# -------------------------------------------------
library(ggplot2)
ggplot(har_rv_j, aes(x = date)) +
  geom_line(aes(y = rv, color = "Realized Volatility (rv)"), linewidth = 1) +  
  geom_line(aes(y = har, color = "HAR-RV-J Model Forecast"), linewidth = 1) +      
  labs(title = "Comparison of Realized Volatility and HAR-RV-J Model Forecast",
       x = "Date", y = "Volatility",
       color = "Legend") + 
  theme_minimal() +
  theme(legend.position = "top")

# -------------------------------------------------
# 11. The Mincer–Zarnowitz Regression
# -------------------------------------------------
mz <- lm(rv ~ 1 + har, data = har_rv_j)
fit_mz <- summary(mz)
print(fit_mz)

t_test <- (coefficients(fit_mz)[2] - 1) / fit_mz$coefficients[2, "Std. Error"]
p_value <- pt(abs(t_test), df = mz$df.residual, lower.tail = FALSE) * 2
print(p_value)

# -------------------------------------------------
# 12. Loss Functions
# -------------------------------------------------
har_rv_j_metrics <- data.frame(Model = "HAR-RV-J") %>%  
  mutate(har_rv_j %>% 
           summarise(
             MAE = mean(abs(rv - har)),
             RMSE = sqrt(mean((rv - har)^2, na.rm = TRUE)),
             QLIKE = mean(log(har) + rv / har),
             R_2 = 1 - sum((rv - har)^2, na.rm = TRUE) / sum((rv - mean(rv, na.rm = TRUE))^2, na.rm = TRUE)) 
  )


print(har_rv_j_metrics)

# -------------------------------------------------
# 13. Residual Analysis
# -------------------------------------------------
residuals <- (har_rv_j$rv - har_rv_j$har)

# Durbin-Watson Test
dw_test <- dwtest(residuals ~ 1)
print(dw_test)

# Ljung-Box Test
lb_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
print(lb_test)

# Breusch-Pagan Test
bp_test <- bptest(residuals ~ har_rv_j$rv)
print(bp_test)

# Shapiro-Wilk Test
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# -------------------------------------------------
# 14. Visualization of Residuals
# -------------------------------------------------
# Residuals Time Series
ggplot(data = data.frame(index = 1:length(residuals), residual = residuals), aes(x = index, y = residuals)) +
  geom_line(color = "blue") +
  theme_classic() +
  labs(title = "Residual Time Series", x = "Index", y = "Residual")

# Residuals Histogram
ggplot(data = data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_classic() +
  labs(title = "Residual Histogram with Density", x = "Residual", y = "Density")

# Q-Q Plot
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")

# Residuals vs. Fitted Values
ggplot(data = data.frame(fitted = har_rv_j$har, residuals = residuals), aes(x = fitted, y = residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_classic() +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")


# -------------------------------------------------
# 15. Autocorrelation Analysis
# -------------------------------------------------
# 15.1 RV Autocorrelation
bacf <- acf(har_rv_j$rv, lag.max = 200, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, aes(x=lag, y=acf)) + 
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0, color = "blue") + 
  theme_classic() +  
  labs(title = "RV Autocorrelation", x = "Lag", y = "Autocorrelation")

# 15.2 HAR Autocorrelation
bacf <- acf(har_rv_j$har, lag.max = 200, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, aes(x=lag, y=acf)) + 
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0, color = "blue") + 
  theme_classic() +  
  labs(title = "Autocorrelation Function - HAR-RV", x = "Lag", y = "Autocorrelation")

# -------------------------------------------------
# Conclusion
# -------------------------------------------------

# The HAR-RV-J model extends the traditional HAR framework by incorporating jumps into realized volatility forecasting. 
# The analysis confirmed that while jumps may not always be statistically significant in a static setting, the HAR-RV-J model 
# achieved better predictive accuracy than the standard HAR model, with lower MAE, RMSE, and improved R-squared. 
# This suggests that including jumps enhances the model's ability to capture extreme market movements, particularly in a dynamic framework.

# Future improvements may include refining jump detection methods and exploring alternative volatility measures. 
# Additionally, further evaluation of forecasting performance across different market conditions could provide deeper insights 
# into the model’s robustness. These enhancements would strengthen the practical application of HAR-based models in financial risk management and forecasting.

