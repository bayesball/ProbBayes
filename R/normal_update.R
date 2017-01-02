normal_update <- function(prior, data){
  prior_mean <- prior[1]
  prior_sd <- prior[2]
  data_mean <- data[1]
  data_se <- data[2]
  prior_precision <- 1 / prior_sd ^ 2
  data_precision <- 1 / data_se ^ 2
  post_precision <- prior_precision + data_precision
  post_sd <- sqrt(1 / post_precision)
  post_mean <- weighted.mean(c(prior_mean, data_mean),
              c(prior_precision, data_precision))
  c(post_mean, post_sd)
}
