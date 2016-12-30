beta_interval <- function(prob, shape_par){
  x <- seq(0, 1, by=.001)
  dx <- dbeta(x, shape_par[1], shape_par[2])
  p <- ggplot(data.frame(x=x, dx=dx), aes(x, dx)) +
    geom_line()
  interval <- qbeta(c((1 - prob) / 2, 1 - (1 - prob) / 2),
                    shape_par[1], shape_par[2])
  lo <- interval[1]
  hi <- interval[2]
  LO <- max(0, lo)
  HI <- min(1, hi)
  x0 <- seq(LO, HI, by=.001)
  y0 <- dbeta(x0, shape_par[1], shape_par[2])
  xx <- c(LO, x0, HI, lo)
  yy <- c(0, y0, 0, 0)
  betaprob <- diff(pbeta(c(lo, hi),
                         shape_par[1], shape_par[2]))
  message <- paste("P(",
                   round(lo, 3), "< P < ", round(hi, 3), ") =",
                   round(betaprob, 3))
  other_text <- paste("Beta(",shape_par[1], ", ",
                      shape_par[2], ")", sep="")

  x_text <- ifelse(shape_par[1] > shape_par[2],
                   .15, .85)
  y_text <- .70 * max(dx)
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  p + geom_polygon(data=data.frame(xx, yy), aes(xx, yy),
                   fill="orange")  +
    ggtitle(message) +
    xlab("P") + ylab("Density") + TH +
    annotate("text", x = x_text, y = y_text,
             label = other_text,
             size=6, color="blue")
}
