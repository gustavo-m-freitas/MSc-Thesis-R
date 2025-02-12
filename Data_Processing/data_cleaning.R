# -----------------------------------------------------------
# Data Cleaning Script
# Author: Gustavo Freitas
# Master's Thesis: Forecasting FTSE-100 Volatility Using HAR-Type Models
# -----------------------------------------------------------

# Load required packages
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(xts)
library(readxl)
library(timeDate)

# -----------------------------------------------------------
# 1. Load Processed Data
# -----------------------------------------------------------
ftse <- read_csv("FTSE_2025.csv", show_col_types = FALSE)
ftse <- ftse %>% mutate(return = log(value / dplyr::lag(value)))
head(ftse, 5)

# -----------------------------------------------------------
# 2. Removing Problematic Dates
# -----------------------------------------------------------
problematic_dates <- c("2000-04-03", "2000-04-05", "2001-03-08", "2001-12-31", 
                       "2003-04-14", "2004-01-23", "2005-01-31", "2005-02-01", 
                       "2007-03-02", "2007-04-02", "2007-04-03", "2007-04-04", 
                       "2007-04-05", "2007-04-10", "2007-08-09", "2008-09-08", "2009-11-26")
ftse <- ftse %>% filter(!as.character(date) %in% problematic_dates)

# -----------------------------------------------------------
# 3. Check for Missing Values
# -----------------------------------------------------------
na_summary <- ftse %>% summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Values")
print(na_summary)

# -----------------------------------------------------------
# 4. Check Trading Days Consistency
# -----------------------------------------------------------
daily_ftse <- ftse %>% 
  group_by(date) %>% 
  summarise(first_time = first(time),
            last_time = last(time), 
            open = first(value), 
            close = last(value), 
            low = min(value), 
            high = max(value), 
            nobs = n())

# Check if weekends are included
daily_ftse %>% mutate(day = wday(date, label = TRUE)) %>% count(day)

# -----------------------------------------------------------
# 5. Count Trading Days Per Year
# -----------------------------------------------------------
trading_days <- ftse %>% 
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(n_days = n_distinct(date)) %>%
  ungroup() %>%
  mutate(diff = 253 - n_days)

print(trading_days)

# Plot a bar chart of trading days per year
ggplot(trading_days, aes(x = factor(year), y = n_days)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +  # Bar plot
  geom_hline(yintercept = 253, color = "red", linetype = "dashed", linewidth = 0.8) +  # Horizontal line at 253
  labs(
    title = "Number of Trading Days Per Year",
    x = "Year",
    y = "Number of Trading Days"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# -----------------------------------------------------------
# 6. Analyze Monthly Trading Activity in 2001
# -----------------------------------------------------------
days_2001 <- daily_ftse %>% 
  filter(date >= "2001-01-01" & date <= "2001-12-31") %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month) %>% summarise(nobs = n())
print(days_2001)

# Plot a bar chart of trading days in 2001
ggplot(days_2001, aes(x = factor(month, levels = 1:12), y = nobs)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +  # Bar plot
  geom_hline(yintercept = 22, color = "red", linetype = "dashed", linewidth = 0.8) +  # Horizontal line at 22
  labs(
    title = "Number of Trading Days in 2001",
    x = "Month",
    y = "Number of Trading Days"
  ) +
  scale_x_discrete(labels = month.name) +  # Use month names as labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# -----------------------------------------------------------
# 7. Analyze Trading Duration Per Day
# -----------------------------------------------------------
time <- daily_ftse %>% 
  select(date, first_time, last_time) %>% 
  mutate(diff = as.numeric(last_time - first_time) / 60 / 60)

summary(time$diff)

# Identify days with longer than 9-hour sessions
print(time %>% filter(diff > 9))

# Identify days with shorter than 8-hour sessions
print(time %>% filter(diff < 8))

# Plot trading session duration over time
ggplot(time, aes(x = date, y = diff)) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_point(color = "darkblue") +
  geom_hline(yintercept = 8, color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 9, color = "green", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Trading Hours Over Time",
    x = "Date",
    y = "Trading Hours"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------------
# 8. Identify Large Time Gaps (>= 15 minutes)
# -----------------------------------------------------------
time_diff <- ftse %>%
  filter(abs(time_diff) >= 60 * 15) %>% 
  group_by(date) %>% 
  filter(n() > 1) %>% slice(-1) %>% ungroup()
print(time_diff)

# Check if timestamps are in increasing order
is_sorted <- all(diff(as.numeric(ftse$timestamp)) >= 0)
message("Are timestamps sorted correctly? ", is_sorted)


# -----------------------------------------------------------
# 9. Check for Negative or Zero Values
# -----------------------------------------------------------
negative_values <- ftse %>% filter(value <= 0) %>% summarise(count = n())
print(negative_values)

# -----------------------------------------------------------
# 10. Number of observations per day
# -----------------------------------------------------------
# Count observations per day
n_obs <- ftse %>% 
  group_by(date) %>%
  summarise(n_obs = n()) 

# Print summary statistics
summary(n_obs$n_obs)

# -----------------------------------------------------------
# 11. Days with Large Trading observations**
# -----------------------------------------------------------
n_obs_high <- n_obs %>% filter(n_obs > 2500)
print(n_obs_high)

# Plot of Daily Trading observations**
ggplot(n_obs, aes(x = date, y = n_obs)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 2500, color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = min(n_obs$date), y = 3500, label = "2500 trades/day", color = "red", hjust = 0) +
  labs(
    title = "Daily Trading Observations Over Time",
    x = "Date",
    y = "Number of Observations"
  ) +
  theme_minimal()


# -----------------------------------------------------------
# 12- Are all observations concentrated at a given time during the day?**
# -----------------------------------------------------------
daily_ftse <- daily_ftse %>%
  mutate(
    first_time_hours = as.numeric(first_time) / 3600,  # Convert seconds to hours
    last_time_hours = as.numeric(last_time) / 3600    # Convert seconds to hours
  )

# Summary statistics for opening and closing times in hours
summary(daily_ftse$first_time_hours)
summary(daily_ftse$last_time_hours)

# -----------------------------------------------------------
# 13- Observation on Opening and Closing Times**
# -----------------------------------------------------------
# Due to the implementation of daylight saving time in the United Kingdom, clusters in 
# opening and closing times with a one-hour difference are expected. This occurs because 
# clocks are adjusted forward by one hour in the spring and backward by one hour in the autumn.

# Plot of Opening Times**
ggplot(daily_ftse, aes(x = date, y = first_time)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Opening Times Over Time",
    x = "Date",
    y = "Opening Time"
  ) +
  theme_minimal()


# Plot of Closing Times**
ggplot(daily_ftse, aes(x = date, y = last_time)) +
  geom_point(color = "green", alpha = 0.6) +
  labs(
    title = "Closing Times Over Time",
    x = "Date",
    y = "Closing Time"
  ) +
  theme_minimal()
# -----------------------------------------------------------
# 14- Check for Consecutive Repeated Values**
# -----------------------------------------------------------
repeated_values <- ftse %>%
  mutate(repeated = value == lag(value)) %>%  # Check if the value is the same 
  filter(repeated == TRUE)  # Filter rows with repeated values

# Group by 'value' and 'date' and summarize
repeated <- repeated_values %>%
  group_by(value, date) %>%
  summarise(
    n_obs = n(),  # Count of repeated observations for each value per date
    .groups = "drop" 
  )

# Reorder columns and arrange by date
repeated <- repeated %>%
  select(date, value, n_obs) %>%  # Ensure columns are in the desired order
  arrange(date)

# Filter for rows where n_obs > 40
repeated %>% filter(n_obs > 40)

# -----------------------------------------------------------
# 15- Analyze Repeated Values on 2009-12-18**
# -----------------------------------------------------------
values <- repeated_values %>% filter(value == 5246.27)

# Calculate the duration of repeated values in minutes
duration_minutes <- as.numeric((values$time[nrow(values)] - values$time[1]) / 60)
# The repeated values on 2009-12-18 lasted for less than 5 minutes
print(duration_minutes)

# Plot for 2009-12-18**
filtered_data <- ftse %>%
  filter(date == as.Date("2009-12-18"))

# Plot the values for the specific date
ggplot(filtered_data, aes(x = timestamp, y = value)) +
  geom_line(color = "blue", linewidth = 0.8) +
  labs(
    title = "Value Changes on 2009-12-18",
    x = "Timestamp",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot value over date
ggplot(ftse %>% sample_n(5000), aes(x = date, y = value)) +
  geom_line(color = "blue", linewidth = 0.8) +  # Add line connecting the values
  labs(
    title = "Value Over Time",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +  # Apply a clean, minimalist theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5) ) # Center the plot title

# -----------------------------------------------------------
# 16- Analyze Large Returns (> 1%) at same day**
# -----------------------------------------------------------
summary(ftse$return * 100)

large_returns <- ftse %>%   filter(abs(return) >= 0.01) %>%  # Filter for large returns
  group_by(date) %>%  
  filter(n() > 1) %>%  # Group by date and select only same day 
  ungroup()  # Ungroup to simplify the resulting dataset

# Print the resulting dataset
print(large_returns)

# -----------------------------------------------------------
# 17- Calculate 5-minute intraday returns**
# -----------------------------------------------------------s 
ftse_5min <- ftse %>% 
  mutate(five_min_ceiling = ceiling_date(timestamp, "5 min")) %>% 
  group_by(five_min_ceiling) %>% 
  summarise(value = last(value)) %>% 
  ungroup() 

ftse_5min <- ftse_5min %>% mutate(
  date = as.Date(five_min_ceiling),  # Extract date from the rounded timestamp
  time = hms::as_hms(five_min_ceiling),  # Extract time as hms
  return = log(value/dplyr::lag(value))  # Calculate return (value variation)
) %>% select(date, time, value, return) %>% 
  dplyr::filter(!is.na(return))

ftse_5min

# Plot the Returns over time
ggplot(ftse_5min %>% sample_n(5000), aes(x = date, y = return)) +
  geom_line(color = "green", linewidth = 0.8) +
  labs(
    title = "Retruns Over Time",
    x = "Date",
    y = "Returns"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5) )


# -----------------------------------------------------------
# 18- Summary 5-minute intraday returns**
# -----------------------------------------------------------
ftse_5min_obs <- ftse_5min %>% group_by(date) %>% summarise(nobs = n())
print(ftse_5min_obs)


# Plot: Number of 5-Minute Observations Per Day**
ggplot(ftse_5min_obs , aes(x = date, y = nobs)) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_point(color = "darkblue", size = 1.5) +
  labs(
    title = "Number of 5-Minute Observations Per Day",
    x = "Date",
    y = "Number of Observations"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# -----------------------------------------------------------
# 19 - Compute daily realized volatility**
# -----------------------------------------------------------
rv <- ftse_5min %>% 
  group_by(date) %>% 
  summarise(rv = sum(return^2))
head(rv)

# Plot: Realized Volatility Over Time**
ggplot(rv, aes(x = date, y = rv)) +
  geom_line(color = "green", linewidth = 0.8) +
  labs(
    title = "Realized Volatility Over Time",
    x = "Date",
    y = "Realized Volatility"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# -----------------------------------------------------------
# Conclusion
# -----------------------------------------------------------
# This document presents the detailed process of cleaning, validating, and analyzing the 
# FTSE-100 volatility dataset as part of the data preparation phase for the master's 
# thesis. The steps included removing problematic dates, verifying data integrity 
# (e.g., handling missing values and ensuring proper sorting of timestamps), and analyzing
# daily and intraday trading activities, while also exploring metrics such as trading 
# session durations, daily trading volumes, and repeated observations to ensure data 
# quality. Additionally, the dataset was further processed by aggregating 5-minute 
# intraday returns to compute realized volatility, setting a strong foundation for 
# implementing advanced HAR-type models for robust and reliable volatility forecasting.
