\name{two_p_update}
\alias{two_p_update}

\title{
  Posterior updating of two proportions
}
\description{
  Computes posterior distribution of two proportions with a discrete prior
}
\usage{
  two_p_update(prior, y1n1, y2n2)
}
\arguments{
  \item{prior}{
    prior probability matrix where the rows and columns are labeled with the values of the proportions
  }
  \item{y1n1}{
  number of successes and sample size from first sample
  }
   \item{y2n2}{
  number of successes and sample size from second sample
  }
}

\value{
  posterior probability matrix
}
\author{
  Jim Albert
}

\examples{
  prior <- testing_prior()
  two_p_update(prior, c(3, 10), c(8, 20))
}
