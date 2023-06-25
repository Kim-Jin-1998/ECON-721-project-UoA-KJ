# Load necessary packages
library(readr)
library(dplyr)
library(plm)

# Import the data
data <- read_csv("data_f.csv")
# Generate log variables
vars_to_log <- c("price_eur_m_whe", "gwh", "cp_meur_nsa_b1g", "cp_meur_nsa_d1", "cp_meur_nsa_d11", "cp_meur_nsa_p6", "cp_meur_nsa_p7", "ths_hw_b_e_nsa_emp_dc", "ths_hw_c_nsa_emp_dc", "ths_hw_j_nsa_emp_dc", "ths_hw_g_i_nsa_emp_dc", "ths_hw_total_nsa_emp_dc", "exp_e7000_gwh")
data <- mutate_at(data, vars_to_log, log)

# Create a factor variable for country and date
data$country <- as.factor(data$country)
data$date <- as.factor(data$date)
#data 
# Set up panel data structure
pdata <- pdata.frame(data, index = c("country", "date"))
#str(pdata)
# Remove missing values from 'logcp_meur_nsa_b1g'
pdata <- pdata[!is.na(pdata$cp_meur_nsa_b1g), ]
#
dim(X)
dim(y)
# Define the response variable y and the matrix of predictors X
# Repeat country-specific 'y' values to match the panel structure
y <- matrix(rep(y, each = nrow(X) / nlevels(pdata$country)), nrow = nrow(X), ncol = ncol(y), byrow = TRUE)

X <- as.matrix(pdata[, vars_to_log])# Calculate the OLS estimates using matrix algebra
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

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

# Define the instrumental variable
# Define the instrumental variable
Z <- as.matrix(cbind(1, lag(pdata$cp_meur_nsa_b1g, 1), pdata$ths_hw_b_e_nsa_emp_dc - pdata$exp_e7000_gwh))

# Define the endogenous variable
X_endog <- pdata$gwh

# Define the exogenous variables
X_exog <- cbind(1, lag(pdata$cp_meur_nsa_b1g, 1), pdata$ths_hw_b_e_nsa_emp_dc - pdata$exp_e7000_gwh)

# Define the dependent variable
Y <- pdata$cp_meur_nsa_b1g

# Create a data frame with all variables
data_all <- data.frame(Y, X_endog, X_exog, Z)
data_all
# Remove rows with missing values
complete_data <- na.omit(data_all)

# Redefine the variables using the complete data
Z <- as.matrix(complete_data[, 4:ncol(complete_data)])
X_endog <- complete_data$X_endog
X_endog
X_exog <- as.matrix(complete_data[, 2:3])
Y <- complete_data$Y

# First stage: regress the endogenous variable on the instruments
beta_hat_1st <- solve(t(Z) %*% Z) %*% t(Z) %*% X_endog

# Print the estimated coefficients from the first stage
print(beta_hat_1st)
# Check for identical columns
for(i in 1:(ncol(Z)-1)) {
  for(j in (i+1):ncol(Z)) {
    if(all(Z[,i] == Z[,j])) {
      print(paste("Columns", i, "and", j, "are identical"))
    }
  }
}

# Check for zero variance
for(i in 1:ncol(Z)) {
  if(var(Z[,i]) == 0) {
    print(paste("Column", i, "has zero variance"))
  }
}

# Check for linear combinations
if(qr(Z)$rank < ncol(Z)) {
  print("Z has linear combinations")
}

# Redefine Z without the problematic columns
Z <- Z[, -c(3, 4, 5)]

# Recalculate the first stage regression
beta_hat_1st <- solve(t(Z) %*% Z) %*% t(Z) %*% X_endog

# Print the estimated coefficients from the first stage
print(beta_hat_1st)


# First stage: regress the endogenous variable on the instruments
beta_hat_1st <- solve(t(Z) %*% Z) %*% t(Z) %*% X_endog
beta_hat_1st
# Obtain the predicted values of the endogenous variable
X_endog_hat <- Z %*% beta_hat_1st
X_endog_hat
# Second stage: regress the dependent variable on the predicted endogenous variable and the exogenous variables
X_2nd <- cbind(X_endog_hat, X_exog)
beta_hat_2nd <- solve(t(X_2nd) %*% X_2nd) %*% t(X_2nd) %*% Y

# Print the estimated coefficients
print(beta_hat_2nd)

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

### GMM regression

# Define the instrumental variable
Z <- as.matrix(cbind(lag(pdata$cp_meur_nsa_b1g, 1), pdata$ths_hw_b_e_nsa_emp_dc - pdata$exp_e7000_gwh))

# Define the endogenous variable
X_endog <- pdata$gwh

# Define the exogenous variables
X_exog <- cbind(1, lag(pdata$cp_meur_nsa_b1g, 1), pdata$ths_hw_b_e_nsa_emp_dc - pdata$exp_e7000_gwh)

# Define the dependent variable
Y <- pdata$cp_meur_nsa_b1g

# Create a data frame with all variables
data_all <- data.frame(Y, X_endog, X_exog, Z)

# Remove rows with missing values
complete_data <- na.omit(data_all)

# Redefine the variables using the complete data
Z <- as.matrix(complete_data[, 4:ncol(complete_data)])
X_endog <- complete_data$X_endog

X_exog <- as.matrix(complete_data[, 2:3])
Y <- complete_data$Y

# Print the dimensions of the matrices
print(dim(X_endog))
print(dim(X_exog))
print(dim(Z))

# Define the matrix of variables
X <- cbind(X_endog, X_exog)

# Define the matrix of instruments
W <- Z
print(dim(W))
print(dim(Y))
length(Y)
print(dim(Z))
# Calculate the GMM estimates using matrix algebra
# Define the weighting matrix as an identity matrix
# Define the weighting matrix as an identity matrix
W <- diag(nrow(Z))
# Check for identical columns
for(i in 1:(ncol(Z)-1)) {
  for(j in (i+1):ncol(Z)) {
    if(all(Z[,i] == Z[,j])) {
      print(paste("Columns", i, "and", j, "are identical"))
    }
  }
}

# Check for zero variance
for(i in 1:ncol(Z)) {
  if(var(Z[,i]) == 0) {
    print(paste("Column", i, "has zero variance"))
  }
}

# Check for linear combinations
if(qr(Z)$rank < ncol(Z)) {
  print("Z has linear combinations")
}
# Perform a QR decomposition of Z
qr_Z <- qr(Z)

# Get the rank of Z
rank_Z <- qr_Z$rank

# Get the number of columns in Z
ncol_Z <- ncol(Z)

# Check if Z has linearly dependent columns
if(rank_Z < ncol_Z) {
  # Z has linearly dependent columns
  print("Z has linearly dependent columns")
  
  # Get the linearly independent columns of Z
  Z_independent <- Z[, qr_Z$pivot[1:rank_Z]]
  
  # Redefine Z as the matrix of linearly independent columns
  Z <- Z_independent
}
# Calculate the GMM estimates using matrix algebra
beta_hat_gmm <- solve(t(Z) %*% W %*% Z) %*% t(Z) %*% W %*% Y

# Print the estimated coefficients
print(beta_hat_gmm)

# Redefine X as the matrix of variables corresponding to the linearly independent columns of Z
X <- cbind(X_endog, X_exog[, 1:rank_Z])

# Calculate the fitted values
y_hat <- Z %*% beta_hat_gmm

# Print the fitted values
print(y_hat)

# Calculate the residuals
residuals <- Y - y_hat

# Calculate R-squared
SSE <- sum(residuals^2)
SST <- sum((Y - mean(Y))^2)
R_squared <- 1 - SSE/SST

# Print R-squared
print(R_squared)


# Calculate the Durbin-Watson statistic
dw_stat <- sum(diff(residuals)^2) / SSE

# Print the Durbin-Watson statistic
print(dw_stat)
