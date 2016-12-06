spinner_data <- function(regions, nsim=1000){
  sample(length(regions), size=nsim,
         prob=regions, replace=TRUE)
}
