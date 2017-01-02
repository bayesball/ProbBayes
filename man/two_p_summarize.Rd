\name{two_p_summarize}
\alias{two_p_summarize}

\title{
  Summaries of a probability matrix
}
\description{
  Computes posterior of difference P1 - P2 of a probability matrix of two proportions
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
  data frame with variables diff12 and Prob
}
\author{
  Jim Albert
}

\examples{
  two_p_summarize(testing_prior())
}
