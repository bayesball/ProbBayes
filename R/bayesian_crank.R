bayesian_crank <- function(d){
  d$Product <- with(d, Likelihood * Prior)
  d$Posterior <- with(d, Product / sum(Product))
  d
}
