twoproplike <- function(y, p1, p2, size1, size2){
  dbinom(y[1], size = size1, prob = p1) *
    dbinom(y[2], size = size2, prob = p2)
}