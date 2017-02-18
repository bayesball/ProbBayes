dsampling <- function(sample_b, pop_N, pop_B, sample_n){
  dhyper(sample_b, m=pop_B, n=pop_N - pop_B, k=sample_n)
}
