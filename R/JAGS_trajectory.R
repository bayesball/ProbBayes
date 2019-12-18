JAGS_trajectory <- function(){
"model {
## sampling
for (j in 1:N){
y[j] ~ dbin(p[j], n[j])
logit(p[j]) <- beta0 + beta1 * (x[j] - 30) +
            beta2 * (x[j] - 30) * (x[j] - 30)
}
## priors
beta0 ~ dnorm(0, 0.0001)
beta1 ~ dnorm(0, 0.0001)
beta2 ~ dnorm(0, 0.0001)
}
"
}
