JAGS_beta_binomial <- function(){
"model {
for (i in 1:N){
y[i] ~ dbin(p[i], n[i])
}
for (i in 1:N){
p[i] ~ dbeta(a, b)
}
a <- mu * eta
b <- (1 - mu) * eta
mu ~ dbeta(mua, mub)
eta <- exp(logeta)
logeta ~ dlogis(logn, 1)
}
"
}
