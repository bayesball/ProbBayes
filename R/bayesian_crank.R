bayesian_crank <- function(d){
  mutate(d,
         Product = Likelihood * Prior,
         Posterior = Product / sum(Product))
}
