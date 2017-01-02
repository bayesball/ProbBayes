two_p_summarize <- function(prob_matrix){
  p1 <- as.numeric(dimnames(prob_matrix)[[1]])
  p2 <- as.numeric(dimnames(prob_matrix)[[2]])
  P1 <- rep(p1, length(p2))
  P2 <- rep(p2, each=length(p1))
  df <- data.frame(P1, P2, PROB=as.vector(prob_matrix))
  p1.LT.p2 <- with(df, sum(PROB[P1 < P2]))
  p1.GT.p2 <- with(df, sum(PROB[P1 > P2]))
  p1.EQ.p2 <- with(df, sum(PROB[P1 == P2]))
  data.frame(P1.LT.P2=p1.LT.p2,
             P1.EQ.P2=p1.EQ.p2,
             P1.GT.P2=p1.GT.p2)
}
