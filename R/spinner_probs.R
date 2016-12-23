spinner_probs <- function(regions){
  data.frame(Spin=1:length(regions),
             Prob=regions / sum(regions))
}
