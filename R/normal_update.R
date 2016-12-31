normal_update <- function(prior, data){
  prior_precision <- 1 / prior$sd ^ 2
  data_precision <- 1 / data$se ^ 2
  post_precision <- prior_precision + data_precision
  list(mean = (prior$mean * prior_precision +
               data$mean * data_precision) / post_precision,
       sd = sqrt(1 / post_precision))
}
