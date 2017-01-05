beta_draw <- function(shape_pars){
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  Title <- paste("Beta(", shape_pars[1], ",",
                 shape_pars[2], ") Curve")
  x <- NULL
  ggplot(data.frame(x=c(0, 1)), aes(x)) +
    stat_function(fun=dbeta, geom="line",
                  color="red", size=2.5,
                  args=list(shape1=shape_pars[1],
                            shape2=shape_pars[2])) +
      ggtitle(Title) + TH +
     xlab("P") + ylab("Density")
}
