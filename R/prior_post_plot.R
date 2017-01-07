prior_post_plot <- function(d, ...){
   N <- dim(d)[1]
   Model <- rep(d[, 1], 2)
   Probability <- c(d$Prior, d$Posterior)
   Type <- c(rep("Prior", N), rep("Posterior", N))
   D1 <- data.frame(Model, Probability, Type)
   p <- ggplot(D1, aes(Model, Probability, color=Type)) +
     geom_segment(aes(xend = Model, yend = 0), size = 15,
               lineend = "butt") +
     facet_wrap(~ Type, ncol=1)
   if(nargs()==2)
      p <- p + xlab(...)
   p
}
