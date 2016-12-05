dspinner <- function(x, Prob){
  # x is a vector of observations from 1 to k
  # Prob is matrix of probabilities M by k
  N <- length(x)
  P <- matrix(1, nrow(Prob), 1)
  for(j in 1:N)
    P <- P * Prob[, x[j]]
  P
}
