prior_post_plot <- function(d, ...){
   require(ggplot2)
   D1a <- data.frame(Model=d[, 1],
                     Probability=d$Prior,
                  Type="Prior")
   D1b <- data.frame(Model=d[, 1],
                  Probability=d$Posterior,
                  Type="Posterior")
   D1 <- rbind(D1a, D1b)
   p <- ggplot(D1, aes(Model, Probability, color=Type)) +
     geom_segment(aes(xend = Model, yend = 0), size = 10,
               lineend = "butt") +
     facet_wrap(~ Type, ncol=1)
   if(nargs()==2)
      p <- p + xlab(...)
   p
}
