JAGS_normal <- function(){
"
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
}
