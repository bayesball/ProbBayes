draw_two_p <- function(prob_matrix, ...){
  args <- list(...)
  p1 <- as.numeric(dimnames(prob_matrix)[[1]])
  p2 <- as.numeric(dimnames(prob_matrix)[[2]])
  P1 <- rep(p1, length(p2))
  P2 <- rep(p2, each=length(p1))
  PROB <- as.vector(prob_matrix)
  Type <- ifelse(P1 == P2, "P1 = P2",
          ifelse(P1 < P2, "P1 < P2", "P1 > P2"))
  df <- data.frame(P1, P2, PROB, Type)
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  p <- ggplot(data=df, aes(P1, P2, size=PROB, color=Type)) +
    geom_point()
  if ("title" %in% names(args))
    p <- p + ggtitle(args$title) + TH
  p
}
