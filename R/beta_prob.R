beta_prob <- function(lo, hi, shape_pars){
  if(lo > hi) stop("First argument must not be larger than the second argument")
  diff(pbeta(c(lo, hi), shape_pars[1], shape_pars[2]))
}