spinner_plot <- function(probs, ...){
   args <- list(...)
   if("values" %in% names(args))
     values <- args$values else
     values <- 1:length(probs)
   Spin <- factor(values)
   y <- probs
   df <- data.frame(Spin, y)
   TH <- theme(
     plot.title = element_text(
       colour = "blue",
       size = 18,
       hjust = 0.5,
       vjust = 0.8,
       angle = 0
     )
   )
   p <- ggplot(df, aes(1, y, fill=Spin)) +
   geom_bar(stat="identity") +
   coord_polar(theta = "y", direction=1) +
     xlab("") + ylab("") +
     theme(
       axis.text.x = element_blank(),
       axis.text.y = element_blank(),
       axis.ticks = element_blank())
   if ("title" %in% names(args))
     p <- p + ggtitle(args$title) + TH
   p
}
