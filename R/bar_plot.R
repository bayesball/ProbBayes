bar_plot <- function(y, ...){
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  Y <- NULL
  p <- ggplot(data.frame(Y=y), aes(Y)) +
  geom_bar(width=0.5, fill="red") +
  ylab("Frequency")
  if (nargs() == 2)
    p <- p + ggtitle(...) + TH
  p
}
