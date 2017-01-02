draw_two_p <- function(prob_matrix, ...){
  args <- list(...)
  p1 <- as.numeric(dimnames(prob_matrix)[[1]])
  p2 <- as.numeric(dimnames(prob_matrix)[[2]])
  P1 <- rep(p1, length(p2))
  P2 <- rep(p2, each=length(p1))
  df <- data.frame(P1, P2, PROB=as.vector(prob_matrix))
  df$Type <- ifelse(df$P1 == df$P2, "P1 = P2",
                    ifelse(df$P1 < df$P2, "P1 < P2", "P1 > P2"))
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  p <- ggplot(df, aes(P1, P2, size=PROB, color=Type)) +
    geom_point()
  if ("title" %in% names(args))
    p <- p + ggtitle(args$title) + TH
  p
}
