beta_prior_post <- function(prior_shapes, post_shapes){
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=dbeta, geom="line",
                aes(color="Prior"), size=1.5,
                args=list(shape1=prior_shapes[1],
                          shape2=prior_shapes[2])) +
  stat_function(fun=dbeta, geom="line",
                aes(color="Posterior"), size=1.5,
                args=list(shape1=post_shapes[1],
                          shape2=post_shapes[2])) +
  scale_colour_manual(values=c("red", "blue")) +
  labs(colour = "Type") +
  ggtitle("Prior-Posterior Plot") + TH +
  xlab("P") + ylab("Density")
}
