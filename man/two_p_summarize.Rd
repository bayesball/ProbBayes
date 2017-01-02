\name{two_p_summarize}
\alias{two_p_summarize}

\title{
  Summaries of a probability matrix
}
\description{
  Computes summaries of a probability matrix of two proportions
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
  data frame with variables P1.LT.P2, P1.EQ.P2, P1.GT.P2
}
\author{
  Jim Albert
}

\examples{
  two_p_summarize(testing_prior())
}
