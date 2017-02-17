testing_prior <- function(lo=.1, hi=.9, n_values=9,
                          pequal=0.5, uniform=FALSE){
  p1 <- seq(lo, hi, length = n_values)
  p2 <- p1
  n_diagonal <- n_values
  n_off_diag <- n_values ^ 2 - n_values
  prior <- matrix(0, n_values, n_values) +
    (1 - pequal) / n_off_diag
  diag(prior) <-  pequal / n_values
  if(uniform==TRUE)
    prior <- 0 * prior + 1 / n_values ^ 2
  dimnames(prior)[[1]] <- p1
  dimnames(prior)[[2]] <- p2
  prior
}
