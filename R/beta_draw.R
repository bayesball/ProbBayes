beta_draw <- function(shape_pars, ...){
  args <- list(...)
  p <- ggplot(data.frame(x=c(0, 1)), aes(x)) +
    stat_function(fun=dbeta, geom="line",
                  args=list(shape1=shape_pars[1],
                            shape2=shape_pars[2]))
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  if ("title" %in% names(args))
    p <- p + ggtitle(args$title) + TH
  p
}
