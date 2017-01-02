normal_draw2 <- function(){
  normal_pars <- c(0, 1)
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  Title <- paste("Normal(M, s) Curve")
  x_lo <- normal_pars[1] - 4 * normal_pars[2]
  x_hi <- normal_pars[1] + 4 * normal_pars[2]
  ggplot(data.frame(x=c(x_lo, x_hi)), aes(x)) +
    stat_function(fun=dnorm, geom="line",
                  color="red", size=2.5,
                  args=list(mean=normal_pars[1],
                            sd=normal_pars[2])) +
      ggtitle(Title) + TH +
     ylab("") + xlab("") +
    scale_x_continuous(breaks = c(-4, -2, 0, 2, 4),
            labels = c("M - 4 s", "M - 2 s", "M",
                       "M + 2 s", "M + 4 s")) +
    theme(axis.text=element_text(size=12),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())
}
