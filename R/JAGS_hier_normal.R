JAGS_hier_normal <- function(){
"model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(mu_j[group[i]], invsigma2)
}
## priors
for (j in 1:J){
mu_j[j] ~ dnorm(mu, invtau2)
}
invsigma2 ~ dgamma(a_s, b_s)
sigma <- sqrt(pow(invsigma2, -1))
## hyperpriors
mu ~ dnorm(mu0, g0)
invtau2 ~ dgamma(a_t, b_t)
tau <- sqrt(pow(invtau2, -1))
}
"
}
