spinner_bayes <- function(list_regions,
                          prior,
                          data,
                          plot=TRUE){
  Letters <- c("A", "B", "C", "D", "E",
               "F", "G", "H", "I")
  n <- length(list_regions)
  prior <- abs(prior) / sum(abs(prior))
  bayes_df <- data.frame(Spinner = Letters[1:n],
                         Prior = prior)

  Like <- spinner_likelihoods(list_regions)
  bayes_df$Likelihood <- dspinner(data, Like)

  bayes_df <- bayesian_crank(bayes_df)
  if(plot==TRUE)
    print(prior_post_plot(bayes_df))
  bayes_df
}
