metropolis <- function(logpost, current, C, iter, ...){
  S <- rep(0, iter)
  n_accept <- 0
  for(j in 1:iter){
    candidate <- runif(1, min=current - C,
                       max=current + C)
    prob <- exp(logpost(candidate, ...) -
                  logpost(current, ...))
    accept <- ifelse(runif(1) < prob, "yes", "no")
    current <- ifelse(accept == "yes",
                      candidate, current)
    S[j] <- current
    n_accept <- n_accept + (accept == "yes")
  }
  list(S=S, accept_rate=n_accept / iter)
}
