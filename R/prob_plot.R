prob_plot <- function(d){
  N <- dim(d)[1]
#  Size <- 100 / N
#  Size <- ifelse(Size > 15, 15, Size)
#  Size <- ifelse(Size < 2, 2, Size)
  Size = 1.5
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  p <- ggplot(d,
      aes_string(colnames(d)[1], colnames(d)[2])) +
    geom_segment(aes_string(xend = colnames(d)[1], yend = 0),
                 size = Size,
                 lineend = "butt",
                 color="red") + TH
  p
}
