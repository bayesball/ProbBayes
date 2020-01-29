two_p_summarize <- function(prob_matrix){
  p1 <- as.numeric(dimnames(prob_matrix)[[1]])
  p2 <- as.numeric(dimnames(prob_matrix)[[2]])
  P1 <- rep(p1, length(p2))
  P2 <- rep(p2, each=length(p1))
  PROB <- as.vector(prob_matrix)
  diff21 <- factor(P2 - P1)
  df <- data.frame(P1, P2, PROB, diff21)
  u_diff21 <- unique(df$diff21)
  prob <- rep(0, length(u_diff21))
  for(j in 1:length(u_diff21)){
    prob[j] <- sum(df$PROB[df$diff21 == u_diff21[j]])
  }
  DF <- data.frame(diff21 = as.numeric(as.character(u_diff21)),
             Prob = prob)
  DF[order(DF$diff21), ]
  #df <- summarize(group_by(df, diff21), Prob=sum(PROB))
  #mutate(df, diff21 = as.numeric(as.character(diff21)))
}
