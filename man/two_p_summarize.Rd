\name{two_p_summarize}
\alias{two_p_summarize}

\title{
  Summaries of a probability matrix
}
\description{
  Computes posterior of difference P2 - P1 of a probability matrix of two proportions
}
\usage{
  two_p_summarize(prob_matrix)
}
\arguments{
  \item{prob_matrix}{
    probability matrix where the rows and columns are labeled with the values of the proportions
  }
}

\value{
  data frame with variables diff21 and Prob where diff21 = P2 - P1
}
\author{
  Jim Albert
}

\examples{
  # use uniform prior over values .2, .3, .4
  prior <- testing_prior(.2, .4, 3, uniform=TRUE)
  two_p_summarize(prior)
}
