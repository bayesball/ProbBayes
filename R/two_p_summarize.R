two_p_summarize <- function(prob_matrix){
  require(dplyr)
  p1 <- as.numeric(dimnames(prob_matrix)[[1]])
  p2 <- as.numeric(dimnames(prob_matrix)[[2]])
  P1 <- rep(p1, length(p2))
  P2 <- rep(p2, each=length(p1))
  df <- data.frame(P1, P2, PROB=as.vector(prob_matrix))
  df <- mutate(df, diff12=factor(P1 - P2))
  df <- summarize(group_by(df, diff12), Prob=sum(PROB))
  mutate(df, diff12 = as.numeric(as.character(diff12)))
}
