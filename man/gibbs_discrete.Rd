\name{gibbs_discrete}
\alias{gibbs_discrete}

\title{
  Gibbs sampling of a bivariate discrete distribution
}
\description{
  Implements Gibbs sampling for an arbitrary bivariate discrete distribution
}
\usage{
  gibbs_discrete(p, i = 1, iter = 1000)
}
\arguments{
  \item{p}{
    matrix defining the probabiity distribution
  }
  \item{i}{
  starting row of the matrix
  }
  \item{iter}{
  number of cycles of algorithm
  }
}

\value{
  matrix of simulated draws from algorithm
}
\author{
  Jim Albert
}

\examples{
p <- matrix(c(4, 3, 2, 1,
              3, 4, 3, 2,
              2, 3, 4, 3,
              1, 2, 3, 4) / 40, 4, 4, byrow = TRUE)
out <- gibbs_discrete(p, 1, 100)
}
