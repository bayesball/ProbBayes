\name{gibbs_normal}
\alias{gibbs_normal}

\title{
  Gibbs sampling of the normal sampling posterior
}
\description{
  Implements Gibbs sampling for normal sampling with independent priors on the mean and precision
}
\usage{
  gibbs_normal(s, P = 0.002, iter = 1000)
}
\arguments{
  \item{s}{
    a list with components y, the observed data, mu0, the prior mean of mu, sigma0, the prior standard deviation of mu, a, the shape parameter of the gamma prior on P, b, the rate parameter of the gamma prior on P
  }
  \item{P}{
  starting value of the precision parameter
  }
  \item{iter}{
  number of iterations
  }
}

\value{
  matrix of simulated draws of (mu, P) from the algorithm
}
\author{
  Jim Albert
}

\examples{
s <- list(y = rnorm(20, 5, 2),
  mu0 = 10, sigma0 = 3, a = 1, b = 1)
out <- gibbs_normal(s, P = 0.01, iter=100)
}
