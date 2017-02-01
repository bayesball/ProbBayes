spinner_probs <- function(regions){
  data.frame(Region = seq_along(regions),
             Prob = regions / sum(regions))
}
