gibbs_betabin <- function(n, a, b, p = 0.5, iter = 1000){
  x <- matrix(0, iter, 2)
  for(k in 1:iter){
    y <- rbinom(1, size = n, prob = p )
    p <- rbeta(1, y + a, n - y + b )
    x[k, ] <- c(y, p)
  }
  x
}
