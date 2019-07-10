\name{gibbs_betabin}
\alias{gibbs_betabin}

\title{
  Gibbs sampling of the beta-binomial distribution
}
\description{
  Implements Gibbs sampling of the beta-binomial distribution
}
\usage{
  gibbs_betabin(n, a, b, p = 0.5, iter = 1000)
}
\arguments{
  \item{n}{
    binomial sample size
  }
  \item{a}{
  first beta shape parameter
  }
  \item{b}{
  second beta shape parameter
  }
  \item{p}{
  starting value of proportion in algorithm
  }
  \item{iter}{
  number of iterations
  }
}

\value{
  matrix of simulated draws from the algorithm
}
\author{
  Jim Albert
}

\examples{
sp <- gibbs_betabin(20, 5, 5, 100)
}
