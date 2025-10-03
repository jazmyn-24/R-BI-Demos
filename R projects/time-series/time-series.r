# ================================
# Business Time Series Forecasting
# Author: Jazmyn Singh
# ================================

# ===============================
# Install & Load Required Packages
# ===============================

# Install if missing
packages <- c("tidyverse", "forecast", "tseries", "ggplot2")

installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p, dependencies = TRUE)
  }
}

# --- Load Libraries ---
library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)

# --- 1. Load Dataset ---
# Example: Monthly Airline Passengers 1949-1960
data("AirPassengers")
ts_data <- AirPassengers

# --- 2. Plot Time Series ---
autoplot(ts_data) + 
  labs(title = "Monthly Air Passengers",
       x = "Year", y = "Passengers") +
  theme_minimal()

# --- 3. Decomposition (Trend, Seasonality, Residuals) ---
decomp <- decompose(ts_data)
autoplot(decomp)

# --- 4. Stationarity Test ---
print(adf.test(ts_data))   # Augmented Dickey-Fuller Test
diff_ts <- diff(ts_data)
autoplot(diff_ts) +
  labs(title="Differenced Series (Stationary)", x="Year", y="Change in Passengers")

# --- 5. Fit ARIMA Model ---
fit <- auto.arima(ts_data)
print(summary(fit))

# --- 6. Forecasting Next 24 Months ---
forecast_vals <- forecast(fit, h=24)
autoplot(forecast_vals) +
  labs(title="Forecast of Air Passengers",
       x="Year", y="Predicted Passengers")

# --- 7. Accuracy Metrics ---
print(accuracy(fit))

# --- 8. Business Insights ---
cat("\n📌 Business Insights:\n")
cat("- Strong upward trend in air travel demand.\n")
cat("- Clear seasonality (peaks during summer months).\n")
cat("- ARIMA forecasts continued growth in demand.\n")
cat("- Airlines can use this for capacity planning, pricing, and staffing.\n")
