JAGS_normal <- function(){

require(runjags)
modelString = "
model{
## sampling
for (i in 1:N) {
  y[i] ~ dnorm(mu, phi)
}
## priors
mu ~ dnorm(mu0, phi0)
phi ~ dgamma(a, b)
sigma <- sqrt(pow(phi, -1))
}
"
y <- buffalo_jan$JAN
N <- length(y)
the_data <- list("y" = y, "N" = N,
                 "mu0"=10, "phi0"=1/3^2,
                 "a"=1,"b"=1)

InitialValues <- list(
  list(mu = 2, phi = 1 / 2 ^ 2),
  list(mu = 30, phi = 1 / 30 ^ 2)
)
run.jags(modelString,
              n.chains = 2,
              data = the_data,
              monitor = c("mu", "sigma"),
              adapt = 1000,
              burnin = 5000,
              sample = 5000,
              inits = InitialValues)
}
