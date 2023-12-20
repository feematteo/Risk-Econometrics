library(quantmod)

#install.packages("rugarch")
library(rugarch)

start <- "2007-07-01"
end <- "2023-11-30"
df_WTI <- getSymbols("CL=F", src = "yahoo", from = start, to = end,auto.assign=FALSE)
df_BRENT <- getSymbols("BZ=F", src = "yahoo", from = start, to = end,auto.assign=FALSE)

WTI_PRICE <- df_WTI$`CL=F.Adjusted`
BRENT_PRICE <- df_BRENT$`BZ=F.Adjusted`

plot(index(WTI_PRICE), coredata(WTI_PRICE), type = "l", col = "blue", xlab = "Date", ylab = "Price", main = "WTI and BRENT Price")
lines(index(BRENT_PRICE), coredata(BRENT_PRICE), col = "orange")
abline(h = 0, lty = 2, col = "red")
legend("bottomleft", legend = c("WTI oil price", "BRENT oil price"), col = c("blue", "orange"), lty = 1, cex = 0.8)


WTI_PRICE <- WTI_PRICE[WTI_PRICE >= 0]
BRENT_PRICE <- BRENT_PRICE[BRENT_PRICE >= 0]

plot(index(WTI_PRICE), coredata(WTI_PRICE), type = "l", col = "blue", xlab = "Date", ylab = "Price", main = "WTI and BRENT Price")
lines(index(BRENT_PRICE), coredata(BRENT_PRICE), col = "orange")
abline(h = 0, lty = 2, col = "red")
legend("bottomleft", legend = c("WTI oil price", "BRENT oil price"), col = c("blue", "orange"), lty = 1, cex = 0.8)



WTI_returns <- diff(log(WTI_PRICE))
BRENT_returns <- diff(log(BRENT_PRICE))

WTI_returns <- na.omit(WTI_returns)
BRENT_returns <- na.omit(BRENT_returns)



acf_WTI <- acf(WTI_returns, plot = FALSE, lag.max = 500)
acf_BRENT <- acf(BRENT_returns, plot = FALSE, lag.max = 500)


# Create ACF plot without the first observation
plot( acf_WTI$acf[-1], type = "l", lty = 2, ylim = c(-0.06, 0.06), main = "ACF Plot (500 Lags)",
     xlab = "Lag", ylab = "Autocorrelation", col = "blue")

# Add ACF plot for BRENT returns
lines( acf_BRENT$acf[-1], type = "h", lty = 2, col = "red")

# Add confidence bands (assuming 95% confidence)
abline(h = c(-1.96/sqrt(length(WTI_returns)), 1.96/sqrt(length(WTI_returns))), col = "red", lty = 2)

# Add legend
legend("topright", legend = c("WTI", "BRENT", "95% Confidence"), col = c("blue", "red", "gray"), lty = c(2, 2, 2))








plot(index(WTI_returns), coredata(WTI_returns), type = "l", col = rgb(0, 0, 1, alpha = 0.6), xlab = "Date", ylab = "Return", main = "WTI and BRENT Return")
lines(index(BRENT_returns), coredata(BRENT_returns), col = rgb(1, 0.5, 0, alpha = 0.7))
legend("bottomleft", legend = c("WTI oil return", "BRENT oil return"), col = c("blue", "orange"), lty = 1, cex = 0.8)

acf(WTI_returns)

WTI_squared_returns <- WTI_returns^2
BRENT_squared_returns <- BRENT_returns^2


acf(WTI_squared_returns, main = "ACF of Squared Returns - WTI", col = "blue")

# Plot ACF of squared returns for BRENT
acf(BRENT_squared_returns, main = "ACF of Squared Returns - BRENT", col = "blue")


#split data ####
WTI_split_index <- floor(length(WTI_returns) * 0.75)
WTI_estimation_set <- WTI_returns[1:WTI_split_index]
WTI_forecast_set <- WTI_returns[(WTI_split_index + 1):length(WTI_returns)]

BRENT_split_index <- floor(length(BRENT_returns) * 0.75)
BRENT_estimation_set <- BRENT_returns[1:BRENT_split_index]
BRENT_forecast_set <- BRENT_returns[(BRENT_split_index + 1):length(BRENT_returns)]

plot(WTI_estimation_set)
plot(BRENT_forecast_set)

#GARCH models ####
model_WTI_est <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
                            mean.model = list(armaOrder = c(1, 1), include.mean = T),
                            distribution.model = "ged")

# Fit the TGARCH model to WTI returns
result_WTI_est <- ugarchfit(spec = model_WTI_est, data = WTI_estimation_set ,solver.control = list(trace = 0))

# Print TGARCH(1,1) with GED distribution for WTI oil
cat("TGARCH(1,1) with GED distribution for WTI oil:\n")
print(result_WTI_est)

plot(sigma(result_WTI_est))


model_BRENT_est <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), submodel = NULL),
                              mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),
                              distribution.model = "ged")

result_BRENT_est <- ugarchfit(spec = model_BRENT_est, data = BRENT_estimation_set)

cat("MA(1)-GARCH(1,1) with GED distribution for BRENT oil:\n")
print(result_BRENT_est)


plot(sigma(result_BRENT_est))
library(ggplot2)
#Graphs qqplot ####

ggplot() +
  geom_line(aes(x = index(WTI_estimation_set), y = sigma(result_WTI_est)^2, color = "WTI"), size = 1) +
  geom_line(aes(x = index(BRENT_estimation_set), y = sigma(result_BRENT_est)^2, color = "BRENT"), size = 1) +
  labs(title = "Conditional Variances Comparison",
       x = "Date",
       y = "Conditional Variance",
       color = "Oil Type") +
  theme_minimal()

# Create a simple plot for WTI

plot(WTI_estimation_set$`CL=F.Adjusted`, type = "l", col = "blue", ylab = "Returns and Conditional Volatility", main = "WTI Returns")
lines( sigma(result_WTI_est), col = "red",lwd=3)


# Create a simple plot for BRENT
plot(BRENT_estimation_set$`BZ=F.Adjusted`, type = "l", col = "blue", ylab = "Returns and Conditional Volatility", main = "BRENT Returns")
lines( sigma(result_BRENT_est), col = "red",lwd=3)



#std residuals ####
# Plot standardized residuals
qqnorm(residuals(result_WTI_est) / sigma(result_WTI_est), col='blue')
qqline(residuals(result_WTI_est) / sigma(result_WTI_est), col = 2, lwd=2)


qqnorm(residuals(result_BRENT_est) / sigma(result_BRENT_est), col='blue')
qqline(residuals(result_BRENT_est) / sigma(result_BRENT_est), col = 2, lwd=2)

# Add a title
title("Standardized Residuals - WTI GARCH Model")



# Function to plot the distribution of standardized residuals
plot_residual_distribution <- function(result, model_name) {
  standardized_residuals <- residuals(result, standardize = TRUE)
  hist(standardized_residuals, main = paste("Distribution of Standardized Residuals -", model_name),
       xlab = "Standardized Residuals", col = "lightblue", border = "black", probability = TRUE,breaks=100)
  lines(density(standardized_residuals), col = "blue", lwd = 2)
  rug(standardized_residuals, col = "red")
  legend("topright", legend = c("Density", "Rug"), col = c("blue", "red"), lwd = c(2, 1))
}


plot_residual_distribution <- function(result, model_name) {
  standardized_residuals <- residuals(result, standardize = TRUE)
  hist(standardized_residuals, main = paste("Distribution of Standardized Residuals -", model_name),
       xlab = "Standardized Residuals", col = "lightblue", border = "black", probability = TRUE, breaks = 100)
  
  # Add density curve in forest green
  lines(density(standardized_residuals), col = "blue", lwd = 2)
  
  # Overlay a line representing the normal distribution with mean and variance of residuals
  normal_line <- seq(min(standardized_residuals), max(standardized_residuals), length = 1000)
  normal_density <- dnorm(normal_line, mean = mean(standardized_residuals), sd = sd(standardized_residuals))
  lines(normal_line, normal_density, col = "red", lwd = 2)
  
  rug(standardized_residuals, col = "forestgreen")
  
  # Add legend
  legend("topright", legend = c("Density", "Rug", "Normal Distribution"), col = c("blue", "forestgreen", "red"), lwd = c(2, 1, 2))
}

plot_residual_distribution(result_WTI_est, "TGARCH")

plot_residual_distribution(result_BRENT_est, "GARCH")


#faccio altro GARCH per BRENT  ##inutile, mantengo il sGARCH(1,1)


model_BRENT_est_2 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), submodel = NULL),
                              mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),
                              distribution.model = "ged")

result_BRENT_est_2 <- ugarchfit(spec = model_BRENT_est_2, data = BRENT_estimation_set)

plot_residual_distribution(result_BRENT_est_2, "gjrGARCH")



#Returns distributions ####


plot_distribution <- function(returns, ticker) {
  hist(returns, main = paste("Distribution Returns -", ticker),
       xlab = "Returns", col = "lightblue", border = "black", probability = TRUE, breaks = 200)
  
  # Add density curve in forest green
  lines(density(returns), col = "blue", lwd = 2)
  
  # Overlay a line representing the normal distribution with mean and variance of residuals
  normal_line <- seq(min(returns), max(returns), length = 1000)
  normal_density <- dnorm(normal_line, mean = mean(returns), sd = sd(returns))
  lines(normal_line, normal_density, col = "red", lwd = 2)
  
  rug(returns, col = "forestgreen")
  
  # Add legend
  legend("topleft", legend = c("Density", "Rug", "Normal Distribution"), col = c("blue", "forestgreen", "red"), lwd = c(2, 1, 2))
}

plot_distribution(WTI_returns, "WTI crude oil")
plot_distribution(BRENT_returns, "BRENT crude oil")

#install.packages("fGarch")

library(fGarch)
quantile_99_WTI <- qged(0.99, mean = 0, sd = 1, nu = 1.5115 )
print(quantile_99_WTI)


window_size <- 250

# # Create a function to calculate rolling VaR using GED quantiles
# calculate_rolling_var_ged <- function(returns, window_size) {
#   var_values <- rollapply(returns, window_size, function(x) qged(0.99, ugarchfit(data = x, spec = ugarchspec(distribution.model = "ged"))))
#   return(var_values)
# }
# 
# library(zoo)
# var_WTI <- calculate_rolling_var_ged(WTI_estimation_set, window_size)
# 
# # Calculate rolling window VaR for BRENT
# var_BRENT <- calculate_rolling_var_ged(BRENT_returns, window_size)


