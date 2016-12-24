beta_data <- function(shape_par, nsim=1000){
  rbeta(nsim, shape_par[1], shape_par[2])
}
