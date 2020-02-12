JAGS_script<- function(model){

  if(model %in% c("beta_binomial",
                  "hier_normal",
                  "hier_trajectory",
                  "normal",
                  "regression",
                  "regression_cond_means",
                  "trajectory") == FALSE){
    stop("Wrong model")
    #    flag <- FALSE
  }

  if (model == "beta_binomial"){
    script <- "model {
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
    }"
  }
  if (model == "hier_normal"){
    script <- "model {
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
}"
  }
  if (model == "hier_trajectory"){
    script <- "model {
for (i in 1:N){
y[i] ~ dbin(p[i], n[i])
logit(p[i]) <- a[player[i]] + b[player[i]] * (x[i] - 30) +
            c[player[i]] * (x[i] - 30) * (x[i] - 30)
}
for (j in 1:J){
a[j] <- B[j,1]
b[j] <- B[j,2]
c[j] <- B[j,3]
B[j,1:3] ~ dmnorm (mu.beta[], Tau.B[,])
}
mu.beta[1:3] ~ dmnorm(mean[1:3],prec[1:3 ,1:3 ])
Tau.B[1:3 , 1:3] ~ dwish(Omega[1:3 ,1:3 ], 3)
}"
  }
  if (model == "normal"){
 script <- "model{
## sampling
for (i in 1:N) {
  y[i] ~ dnorm(mu, phi)
}
## priors
mu ~ dnorm(mu0, phi0)
phi ~ dgamma(a, b)
sigma <- sqrt(pow(phi, -1))
}"
  }
  if (model == "regression"){
  script <- "model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1*x[i], invsigma2)
}
## priors
beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}"
  }
  if (model == "regression_cond_means"){
    script <- "model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1*x[i], invsigma2)
}
## priors
beta1 <- (mu2 - mu1) / (x2 - x1)
beta0 <- mu1 - x1 * (mu2 - mu1) / (x2 - x1)
mu1 ~ dnorm(m1, s1)
mu2 ~ dnorm(m2, s2)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}"
  }
  if (model == "trajectory"){
    script = "model {
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
}"
  }

script
}
