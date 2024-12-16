library(quantmod)  # For stock data
library(tidyverse) # For data manipulation
library(ggplot2)


# Fetch GME historical data for 2020
getSymbols("GME", src = "yahoo", from = "2020-01-01", to = "2020-12-31", auto.assign = TRUE)
gme_prices <- Cl(GME)  # Extract adjusted close prices

# Calculate daily log returns
gme_returns <- dailyReturn(gme_prices, type = "log")
colnames(gme_returns) <- "GME_Returns"

# Risk-free rate (assume a constant daily rate, e.g., 0.01% annualized rate)
risk_free_rate <- (1 + 0.0001)^(1/252) - 1  # Convert to daily


# Function to calculate portfolio metrics
portfolio_metrics <- function(weight_gme, gme_returns, risk_free_rate) {
  # Portfolio return
  portfolio_return <- weight_gme * mean(gme_returns) + (1 - weight_gme) * risk_free_rate
  
  # Portfolio variance
  portfolio_variance <- (weight_gme^2) * var(gme_returns)
  
  # Sharpe Ratio
  sharpe_ratio <- (portfolio_return - risk_free_rate) / sqrt(portfolio_variance)
  
  return(list(return = portfolio_return, variance = portfolio_variance, sharpe = sharpe_ratio))
}

# Optimization function for Sharpe Ratio
optimize_sharpe <- function(gme_returns, risk_free_rate) {
  optimize(function(weight_gme) {
    -portfolio_metrics(weight_gme, gme_returns, risk_free_rate)$sharpe
  }, interval = c(-2, 1))  # Allow for short-selling (negative weights)
}

# Run optimization
opt_result <- optimize_sharpe(coredata(gme_returns), risk_free_rate)

# Extract optimal weight
optimal_weight_gme <- opt_result$minimum
optimal_metrics <- portfolio_metrics(optimal_weight_gme, coredata(gme_returns), risk_free_rate)

# Print results
cat("Optimal Weight for GME (short-selling):", optimal_weight_gme, "\n")
cat("Portfolio Return:", optimal_metrics$return, "\n")
cat("Portfolio Variance:", optimal_metrics$variance, "\n")
cat("Sharpe Ratio:", optimal_metrics$sharpe, "\n")

# Sensitivity analysis for Sharpe Ratio
weights <- seq(-2, 1, length.out = 100)
sharpe_ratios <- sapply(weights, function(w) {
  portfolio_metrics(w, coredata(gme_returns), risk_free_rate)$sharpe
})


# Plot Sharpe Ratio vs Weight
plot(weights, sharpe_ratios, type = "l", main = "Sharpe Ratio vs GME Weight",
     xlab = "Weight in GME", ylab = "Sharpe Ratio")
abline(v = optimal_weight_gme, col = "red", lty = 2)


### GME shares ####
# Fetch GME stock data from Yahoo Finance
getSymbols("GME", src = "yahoo", from = "2020-01-01", to = "2023-12-31", auto.assign = TRUE)
gme_prices_historical <- Cl(GME)  # Extract adjusted close prices
gme_data_historical <- data.frame(Date = index(gme_prices_historical), Price = coredata(gme_prices_historical))

head(gme_data_historical)

# Create a line plot using ggplot2
ggplot(gme_data_historical, aes(x = Date, y = GME.Close)) +
  geom_line(color = "blue") +
  labs(title = "GME Stock Price (Jan 2019 - Dec 2021)",
       x = "Date",
       y = "Price (USD)") +
  theme_minimal()
