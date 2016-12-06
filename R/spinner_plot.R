spinner_plot <- function(regions, ...){
  s <- 1:length(regions)
  f <- regions
  locs <- (sum(f) - cumsum(f)) + f / 2
  spins <- rep(s, f)
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  p <- ggplot(data.frame(Spin=spins),
              aes(x = factor(1), fill=factor(Spin))) +
    geom_bar(width=1) +
    annotate("text", x=1, y=locs,
             label=1:length(regions), size=12) +
    coord_polar(theta = "y", direction=1) +
    theme(legend.position="none") + xlab("") + ylab("") +
    scale_fill_brewer(palette = "Set1")
  if (nargs() == 2)
    p <- p + ggtitle(...) + TH
  p
}
