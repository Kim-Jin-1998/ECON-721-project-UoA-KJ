# Load necessary packages
library(readr)
library(dplyr)
library(plm)

# Import the data
data <- read_csv("data_f.csv")
data

# Generate log variables
vars_to_log <- c( "gwh", "cp_meur_nsa_b1g", "cp_meur_nsa_d1", "cp_meur_nsa_d11", "cp_meur_nsa_p6", "cp_meur_nsa_p7", "ths_hw_b_e_nsa_emp_dc", "ths_hw_c_nsa_emp_dc", "ths_hw_j_nsa_emp_dc", "ths_hw_g_i_nsa_emp_dc", "ths_hw_total_nsa_emp_dc", "exp_e7000_gwh")
data <- mutate_at(data, vars_to_log, log)

# Create a factor variable for country and date
data$country <- as.factor(data$country)
data$date <- as.factor(data$date)

# Set up panel data structure
pdata <- pdata.frame(data, index = c("country","date"))
pdata

# Define the response variable y and the matrix of predictors X
y <-pdata$price_eur_m_whe
X <- as.matrix(cbind(1, lag(pdata$cp_meur_nsa_b1g, 1), pdata$gwh, pdata$ths_hw_b_e_nsa_emp_dc, pdata$ths_hw_c_nsa_emp_dc, pdata$ths_hw_j_nsa_emp_dc, pdata$ths_hw_g_i_nsa_emp_dc, pdata$ths_hw_total_nsa_emp_dc, pdata$exp_e7000_gwh))
X
y



# Calculate the OLS estimates using matrix algebra
beta_hat <- (t(X) %*% X)^-1 %*% t(X) %*% y

# Print the estimated coefficients
print(beta_hat)

# Create a matrix for the fixed effects
n <- nrow(X)
k <- length(unique(pdata$country))
Z <- kronecker(diag(k), matrix(1, n/k, 1))

# Combine the predictors and the fixed effects
X_fe <- cbind(X, Z)

# Calculate the OLS estimates using matrix algebra
beta_hat_fe <- solve(t(X_fe) %*% X_fe) %*% t(X_fe) %*% y

# Print the estimated coefficients
print(beta_hat_fe)

# Calculate the fitted values
y_hat <- X_fe %*% beta_hat_fe

# Calculate the residuals
residuals <- y - y_hat

# Calculate R-squared
SSE <- sum(residuals^2)
SST <- sum((y - mean(y))^2)
R_squared <- 1 - SSE/SST

# Print R-squared
print(R_squared)

# Calculate the standard errors of the coefficients
k_fe <- ncol(X_fe)
sigma_hat <- sqrt(SSE / (n - k_fe))
var_beta_hat_fe <- sigma_hat^2 * solve(t(X_fe) %*% X_fe)
se_beta_hat_fe <- sqrt(diag(var_beta_hat_fe))

# Print the standard errors
print(se_beta_hat_fe)

# Perform a t-test for each coefficient
t_values <- beta_hat_fe / se_beta_hat_fe
p_values <- 2 * pt(-abs(t_values), df = n - k_fe)

# Print the t-values and p-values
print(t_values)
print(p_values)

# Calculate the Durbin-Watson statistic
dw_stat <- sum(diff(residuals)^2) / SSE

# Print the Durbin-Watson statistic
print(dw_stat)
### 2SLS regresion

# Calculate the fitted values
y_hat <- X_2nd %*% beta_hat_2nd

# Calculate the residuals
residuals <- Y - y_hat

# Calculate R-squared
SSE <- sum(residuals^2)
SST <- sum((Y - mean(Y))^2)
R_squared <- 1 - SSE/SST

# Print R-squared
print(R_squared)

# Calculate the standard errors of the coefficients
n <- nrow(X_2nd)
k <- ncol(X_2nd)
sigma_hat <- sqrt(SSE / (n - k))
var_beta_hat_2nd <- sigma_hat^2 * solve(t(X_2nd) %*% X_2nd)
se_beta_hat_2nd <- sqrt(diag(var_beta_hat_2nd))

# Print the standard errors
print(se_beta_hat_2nd)

# Perform a t-test for each coefficient
t_values <- beta_hat_2nd / se_beta_hat_2nd
p_values <- 2 * pt(-abs(t_values), df = n - k)

# Print the t-values and p-values
print(t_values)
print(p_values)

# Calculate the Durbin-Watson statistic
dw_stat <- sum(diff(residuals)^2) / SSE

# Print the Durbin-Watson statistic
print(dw_stat)

# Perform a Breusch-Pagan test for heteroskedasticity
bp_test <- n * var(residuals) / sum((residuals - mean(residuals))^2)
p_value_bp <- 1 - pchisq(bp_test, df = k)

# Print the Breusch-Pagan test statistic and p-value
print(bp_test)
print(p_value_bp)


### GMM regression

# Define the instrumental variable
Z <- as.matrix(cbind(lag(pdata$price_eur_m_whe, 1), pdata$gwh, pdata$ths_hw_b_e_nsa_emp_dc, pdata$ths_hw_c_nsa_emp_dc, pdata$ths_hw_j_nsa_emp_dc, pdata$ths_hw_g_i_nsa_emp_dc, pdata$ths_hw_total_nsa_emp_dc, pdata$exp_e7000_gwh))

# Define the endogenous variable
X_endog <- pdata$loggwh

# Define the exogenous variables
X_exog <- cbind(1, lag(pdata$price_eur_m_whe, 1), pdata$gwh, pdata$ths_hw_b_e_nsa_emp_dc, pdata$ths_hw_c_nsa_emp_dc, pdata$ths_hw_j_nsa_emp_dc, pdata$ths_hw_g_i_nsa_emp_dc, pdata$ths_hw_total_nsa_emp_dc, pdata$exp_e7000_gwh)

# Define the dependent variable
Y <- pdata$price_eur_m_whe

# Define the matrix of instruments
W <- Z

# Define the matrix of variables
X <- cbind(X_endog, X_exog)

# Calculate the GMM estimates using matrix algebra
beta_hat_gmm <- solve(t(X) %*% W %*% Z %*% X) %*% t(X) %*% W %*% Z %*% Y

# Print the estimated coefficients
print(beta_hat_gmm)

# Calculate the fitted values
y_hat <- X %*% beta_hat_gmm

# Calculate the residuals
residuals <- Y - y_hat

# Calculate R-squared
SSE <- sum(residuals^2)
SST <- sum((Y - mean(Y))^2)
R_squared <- 1 - SSE/SST

# Print R-squared
print(R_squared)

# Calculate the standard errors of the coefficients
n <- nrow(X)
k <- ncol(X)
sigma_hat <- sqrt(SSE / (n - k))
var_beta_hat_gmm <- sigma_hat^2 * solve(t(X) %*% W %*% Z %*% X)
se_beta_hat_gmm <- sqrt(diag(var_beta_hat_gmm))

# Print the standard errors
print(se_beta_hat_gmm)

# Perform a t-test for each coefficient
t_values <- beta_hat_gmm / se_beta_hat_gmm
p_values <- 2 * pt(-abs(t_values), df = n - k)

# Print the t-values and p-values
print(t_values)
print(p_values)

# Calculate the Durbin-Watson statistic
dw_stat <- sum(diff(residuals)^2) / SSE

# Print the Durbin-Watson statistic
print(dw_stat)
