gibbs_normal <- function(s, P = 0.002, iter = 1000){
  ybar <- mean(s$y)
  n <- length(s$y)
  x <- matrix(0, iter, 2)
  for(k in 1:iter){
    mu1 <- (n * P * ybar + s$mu0 / s$sigma0 ^ 2) /
      (n * P + 1 / s$sigma0 ^ 2)
    sigma1 <- sqrt(1 / (n * P + 1 / s$sigma0 ^ 2))
    mu <- rnorm(1, mean = mu1, sd = sigma1)
    a1 <- n / 2 + s$a
    b1 <- sum((s$y - mu) ^ 2) / 2 + s$b
    P <- rgamma(1, shape = a1, rate = b1)
    x[k, ] <- c(mu, P)
  }
  x
}
