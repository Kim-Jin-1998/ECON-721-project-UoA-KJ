library(plm)
library(lmtest)
library(ivreg)
library(AER)
library(stargazer)

# Import the data
data1 <- read.csv("data_f.csv")

# Generate log-transformed variables
data1$logprince <- log(data1$price_eur_m_whe)
data1$loggwh <- log(data1$gwh)
data1$logcp_meur_nsa_b1g <- log(data1$cp_meur_nsa_b1g)
data1$logcp_meur_nsa_d1 <- log(data1$cp_meur_nsa_d1)
data1$logcp_meur_nsa_d11 <- log(data1$cp_meur_nsa_d11)
data1$logcp_meur_nsa_p6 <- log(data1$cp_meur_nsa_p6)
data1$logcp_meur_nsa_p7 <- log(data1$cp_meur_nsa_p7)
data1$logths_hw_b_e_nsa_emp_dc <- log(data1$ths_hw_b_e_nsa_emp_dc)
data1$logths_hw_c_nsa_emp_dc <- log(data1$ths_hw_c_nsa_emp_dc)
data1$logths_hw_j_nsa_emp_dc <- log(data1$ths_hw_j_nsa_emp_dc)
data1$logths_hw_g_i_nsa_emp_dc <- log(data1$ths_hw_g_i_nsa_emp_dc)
data1$logths_hw_total_nsa_emp_dc <- log(data1$ths_hw_total_nsa_emp_dc)
data1$logexp_e7000_gwh <- log(data1$exp_e7000_gwh)

# Panel data analysis
pdata <- pdata.frame(data1, index=c("country", "date"))
pdata
# Descriptive statistics
desc_stats <- summary(pdata[, c("price_eur_m_whe", "gwh", "cp_meur_nsa_b1g", "cp_meur_nsa_d1", "cp_meur_nsa_d11", "cp_meur_nsa_p6", "cp_meur_nsa_p7", "exp_e7000_gwh")])
desc_statss <- as.data.frame(desc_stats)

# Export descriptive statistics
write.csv(desc_stats, "descriptive_stats.csv", row.names = FALSE)
#install.packages("urca")
library("urca")
# Unit root tests
unit_root_tests <- lapply(pdata[, c("price_eur_m_whe", "loggwh", "logcp_meur_nsa_b1g", "logcp_meur_nsa_d1", "logcp_meur_nsa_d11", "logcp_meur_nsa_p6", "logcp_meur_nsa_p7", "logths_hw_b_e_nsa_emp_dc", "logexp_e7000_gwh")], function(x) { ur.df(x, type="none", lags=1) })
unit_root_results <- lapply(unit_root_tests, summary)

# Export unit root test results
unit_root_results <- lapply(unit_root_results, function(x) { as.data.frame(x@testreg$coefficients) })
names(unit_root_results) <- c("price_eur_m_whe", "loggwh", "logcp_meur_nsa_b1g", "logcp_meur_nsa_d1", "logcp_meur_nsa_d11", "logcp_meur_nsa_p6", "logcp_meur_nsa_p7", "logths_hw_b_e_nsa_emp_dc", "logexp_e7000_gwh")

# Export unit root test results
# Unit root tests
unit_root_tests <- lapply(pdata[, c("price_eur_m_whe", "loggwh", "logcp_meur_nsa_b1g", "logcp_meur_nsa_d1", "logcp_meur_nsa_d11", "logcp_meur_nsa_p6", "logcp_meur_nsa_p7", "logths_hw_b_e_nsa_emp_dc", "logexp_e7000_gwh")], function(x) { ur.df(x, type="none", lags=1) })
unit_root_results <- lapply(unit_root_tests, summary)
desc_stats                                                  
desc_statss
unit_root_results
unit_root_tests

# Regression
model_m01 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata, model = "within")
model_m02 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata, model = "between")
model_m03 <- ivreg(logcp_meur_nsa_b1g ~  loggwh + logexp_e7000_gwh, data = pdata)
model_m04 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata, model = "random", effect = "twoways", method = "bg", index = c("country", "date"))

# Save estimation results
est_store <- list(m01 = model_m01, m02 = model_m02, m03 = model_m03, m04 = model_m04)

summary(model_m01)
summary(model_m02)
summary(model_m03)
summary(model_m04)
# Output results to RTF file
#stargazer(est_store, type = "text", out = "model1.txt")

# Repeat for the second set of models
model_m05 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata, model = "random", effect = "twoways", index = c("country", "date"))
model_m06 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata, model = "random", effect = "twoways", index = c("country", "date"), modeler = "within")
model_m07 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata, model = "random", effect = "twoways", index = c("country", "date"), modeler = "between")
model_m08 <- plm(logcp_meur_nsa_b1g ~ lag(logcp_meur_nsa_b1g) + loggwh, data = pdata)
# Repeat for the second set of models
models_set2 <- list(  model_m01,model_m02,model_m03,model_m04)

summary(model_m05)
summary(model_m06)
summary(model_m07)
summary(model_m08)
# Output results to RTF file
#stargazer(models_set2, type = "text", out = "model2.txt")

                 
                 
                 
                 
                 
                 
