library(tidyverse)
library(rlang)
library(furrr)
library(momentfit)
library(tseries)

project<- read.csv("final_data.csv")

y= project$price_eur_m_whe

acf(y)  #autocorrelation
acf((y-mean(y))^2) #Heteroscedasticity 
adf.test(y) #stationary, Time series are stationary if they do not have trend or seasonal effects


gmm_2sls <- function(x, y, z) {
  x_hat <- z %*% solve(t(z) %*% z) %*% t(z) %*% x
  theta <- solve(t(x_hat) %*% x_hat) %*% t(x_hat) %*% y
  as.numeric(theta)
}


#check correlation and delete the variable has correlation greater than 0.8 (if you want to change another correlation, change 0.8 in the loop with other correlation you want)
del_mult_coll <- function(x) {
  x <- as.matrix(x)
  while (any(cor(x) > .8 & cor(x) < 1, na.rm = TRUE)) {
    i <- which(cor(x) > .8 & cor(x) < 1)[1] %% ncol(x)
    x <- x[, -i]
  }
  x
}

#define a set include all X and Z at first, if it have time variable, use -1, -2, and use time in first column and y in second column, if there has no time variable, only use ,1 and make sure y is in first colum
allxz <- del_mult_coll(select(project, -1, -2))

#The idea of the model is to first set up a total XZ dataset that we need, then bring in loop, which will calculate and find all the results that satisfy both the coefficient p-value and hesen test, and then store them in the allmodel set

gmm_model_select_n <- function(n, all_xz, y, data) {
  z_c <- combn(seq_len(ncol(all_xz)), n, NULL, FALSE)
  x_c <- list_c(map(seq_len(n - 1), \(i) {
    combn(seq_len(ncol(all_xz)), i, NULL, FALSE)
  }))
  all_comb <- expand_grid(seq_len(length(z_c)), seq_len(length(x_c)))
  plan(multisession, workers = 10) #Multi-threaded calls, run with as many cores as the computer needs, my test computer is a ten core cpu, so = 10
  all_mods <- array_branch(all_comb, 1) |>
    future_map(\(i) {
      z <- cbind(1, all_xz[, z_c[[i[1]]]])
      x <- cbind(1, all_xz[, x_c[[i[2]]]])
      theta <- try(gmm_2sls(x, y, z), silent = TRUE)
      if (inherits(theta, "try-error")) {
        j_s <- Inf
        j_s_p_val <- NULL
        p_val <- NULL
        se <- NULL
      } else {
        names(theta) <- c("(Intercept)", colnames(x)[-1])
        formula_x <- "price_eur_m_whe" |>
          paste(paste(colnames(all_xz)[x_c[[i[2]]]], collapse = " + "), sep = " ~ ") |>
          as.formula()
        formula_z <- paste("~", paste(colnames(all_xz)[z_c[[i[1]]]], collapse = " + ")) |>
          as.formula()
        test <- gmmFit(momentModel(formula_x, formula_z, data = data))
        p_val <- summary(test)@coef[, 4]
        se <- summary(test)@coef[, 2]
        j_s <- specTest(test)@test[, "Statistics"]
        j_s_p_val <- specTest(test)@test[, "pvalue"]
        if (sum(p_val > .05) > 1L) j_s <- Inf
      }
      list(
        x = colnames(x)[-1], z = colnames(z)[-1], j_s = j_s, j_s_p_val = j_s_p_val,
        theta = theta, p_val = p_val, se = se
      )
    })
  plan(sequential)
  all_mods
}

gmm_model_select <- function(all_xz, y, data) {
  all_mods <- list_c(map(
    seq_len(ncol(all_xz))[-1],
    gmm_model_select_n, all_xz, y, data
  ))
  j_ss <- map_dbl(all_mods, \(x) x$j_s)
  all_mods[order(j_ss)]
}



allmodel<-gmm_model_select(allxz,y,project)
allmodel[[1]]
with(allmodel[[1]], theta -1.96 * se) #CI, lower
with(allmodel[[1]], theta + 1.96 * se) #upper





