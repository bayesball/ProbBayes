gibbs_discrete <- function(p, i = 1, iter = 1000){
  x <- matrix(0, iter, 2)
  nX <- dim(p)[1]
  nY <- dim(p)[2]
  for(k in 1:iter){
    j <- sample(1:nY, 1, prob = p[i, ])
    i <- sample(1:nX, 1, prob = p[, j])
    x[k, ] <- c(i, j)
  }
  x
}
