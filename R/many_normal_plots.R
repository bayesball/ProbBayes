many_normal_plots <- function(list_normal_par){
  N <- length(list_normal_par)
  Means <- sapply(list_normal_par, function(y) y[1])
  SDs <- sapply(list_normal_par, function(y) y[2])
  Means <- round(Means, 1)
  SDs <- round(SDs, 1)
  gmin <- min(Means - 4 * SDs)
  gmax <- max(Means + 4 * SDs)
  labels <- paste("N(", Means, ", ", SDs, ")", sep="")
  x <- seq(gmin, gmax, length.out=200)
  df <- NULL
  for (j in 1:N)
    df <- rbind(df,
          data.frame(Model=labels[j],
                     y=x, f=dnorm(x, Means[j],
                                  SDs[j])))
  ggplot(df, aes(y, f, group=Model, color=Model)) +
    geom_line(size=1.5) +
    scale_colour_manual(values = c("blue", "red"))
}
