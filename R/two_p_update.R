two_p_update <- function(prior, y1n1, y2n2){
  p1 <- as.numeric(dimnames(prior)[[1]])
  p2 <- as.numeric(dimnames(prior)[[2]])
  like1 <- dbinom(y1n1[1], size=y1n1[2], prob=p1)
  like2 <- dbinom(y2n2[1], size=y2n2[2], prob=p2)
  likelihood <- outer(like1, like2)
  product <- prior * likelihood
  product / sum(product)
}
