prob_plot <- function(d){
  p <- ggplot(d,
      aes_string(colnames(d)[1], colnames(d)[2])) +
    geom_segment(aes_string(xend = colnames(d)[1], yend = 0),
                 size = 10,
                 lineend = "butt",
                 color="red")
  p
}
