bayesian_crank <- function(d){
  d$Product <- d$Likelihood * d$Prior
  d$Posterior <- d$Product / sum(d$Product)
  d
}