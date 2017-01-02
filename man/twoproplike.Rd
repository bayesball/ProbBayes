\name{twoproplike}
\alias{twoproplike}

\title{
  Likelihood for two proportions
}
\description{
  Computes likelihood function of two proportions with binomial sampling
}
\usage{
  twoproplike(y, p1, p2, size1, size2)
}
\arguments{
  \item{y}{
    vector of successes of two samples
  }
  \item{p1}{
    vector of values of first probability
  }
   \item{p2}{
  vector of values of second probability
  }
  \item{size1}{
  first sample size
  }
  \item{size2}{
  second sample size
  }
}

\value{
  vector of likelihoods
}
\author{
  Jim Albert
}

\examples{
  p1 <- c(.1, .2, .3)
  p2 <- c(.3, .4, .5)
  twoproplike(c(2, 4), p1, p2, 10, 10)
}
