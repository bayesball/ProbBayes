\name{random_walk}
\alias{random_walk}

\title{
  Metropolis sampling of a discrete distribution
}
\description{
  Implements Metropolis sampling for an arbitrary discrete probability distribution
}
\usage{
  random_walk(pd, start, num_steps)
}
\arguments{
  \item{pd}{
    function containing discrete probability function on the integers 1, 2, ...
  }
  \item{start}{
  starting value of algorithm
  }
  \item{num_steps}{
  number of iterations of algorithm
  }
}

\value{
  A vector of simulated values
}
\author{
  Jim Albert
}

