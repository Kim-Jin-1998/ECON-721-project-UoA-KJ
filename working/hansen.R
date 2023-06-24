library(tidyverse)
library(matrixcalc)

hansen_test <- function(theta, x, z, y, y_fitted) {
  n <- nrow(x)
  m <- map2_dbl(theta, seq_along(theta), \(t, i) {
    mean((y - x %*% theta) * z[, i])
  })
  e <- y - x %*% theta
  S_hat <- as.numeric(t(e^2) %*% e^2 / n^2) * (t(z) %*% z)
  j_stat <- n * t(m) %*% matrix.inverse(S_hat) %*% m
  p_val <- pchisq(j_stat, 1, lower.tail = FALSE)
  print(hansen_j_stat)
  p_val
}
