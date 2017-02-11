normal_draw <- function(normal_pars){
  normal_pars <- unlist(normal_pars)
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  Title <- paste("Normal(", normal_pars[1], ",",
                 normal_pars[2], ") Curve")
  x_lo <- normal_pars[1] - 4 * normal_pars[2]
  x_hi <- normal_pars[1] + 4 * normal_pars[2]
  x <- NULL
  ggplot(data.frame(x=c(x_lo, x_hi)), aes(x)) +
    stat_function(fun=dnorm, geom="line",
                  color="red", size=2.5,
                  args=list(mean=normal_pars[1],
                            sd=normal_pars[2])) +
      ggtitle(Title) + TH +
     ylab("Density")
}
