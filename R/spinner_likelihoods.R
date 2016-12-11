spinner_likelihoods <- function(regions){
  # regions is a list of vectors
  # each vector is a set of spinner areas
  # output is the likelhood matrix, each row corresponds to a spinner
  N <- length(regions)
  n <- max(do.call(rbind, lapply(regions, length)))
  likelihood <- matrix(0, N, n)
  for (j in 1:N){
    nj <- length(regions[[j]])
    likelihood[j, 1:nj] <- regions[[j]] / sum(regions[[j]])
  }
  Letters <- c("A", "B", "C", "D", "E",
               "F", "G", "H", "I", "J")
  dimnames(likelihood)[[1]] <- paste("Spinner", Letters[1:N])
  dimnames(likelihood)[[2]] <- 1:n
  likelihood
}
