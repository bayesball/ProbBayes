JAGS_hier_trajectory <- function(){
"model {
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
}
"
}
