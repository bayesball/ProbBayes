two_p_update <- function(prior, s1f1, s2f2){
  p1 <- as.numeric(dimnames(prior)[[1]])
  p2 <- as.numeric(dimnames(prior)[[2]])
  like1 <- dbinom(s1f1[1], size=s1f1[1] + s1f1[2], prob=p1)
  like2 <- dbinom(s2f2[1], size=s2f2[1] + s2f2[2], prob=p2)
  likelihood <- outer(like1, like2)
  product <- prior * likelihood
  product / sum(product)
}
