library(tidyverse)
library(rlang)
library(furrr)
library(momentfit)
library(tseries)

project<- read.csv("final_data.csv")

y= project$docvis




gmm_2sls <- function(x, y, z) {
  x_hat <- z %*% solve(t(z) %*% z) %*% t(z) %*% x
  theta <- solve(t(x_hat) %*% x_hat) %*% t(x_hat) %*% y
  as.numeric(theta)
}



del_mult_coll <- function(x) {
  x <- as.matrix(x)
  while (any(cor(x) > .8 & cor(x) < 1, na.rm = TRUE)) {
    i <- which(cor(x) > .8 & cor(x) < 1)[1] %% ncol(x)
    x <- x[, -i]
  }
  x
}


allxz <- del_mult_coll(select(project, -1))



gmm_model_select_n <- function(n, all_xz, y, data) {
  z_c <- combn(seq_len(ncol(all_xz)), n, NULL, FALSE)
  x_c <- list_c(map(seq_len(n - 1), \(i) {
    combn(seq_len(ncol(all_xz)), i, NULL, FALSE)
  }))
  all_comb <- expand_grid(seq_len(length(z_c)), seq_len(length(x_c)))
  plan(multisession, workers = 10)
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
        formula_x <- "docvis" |>
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


