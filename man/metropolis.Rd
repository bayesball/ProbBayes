\name{metropolis}
\alias{metropolis}

\title{
  Metropolis sampling of a continuous distribution
}
\description{
  Implements Metropolis sampling for an arbitrary continuous probability distribution
}
\usage{
  metropolis(logpost, current, C, iter, ...)
}
\arguments{
  \item{logpost}{
    function definition of the log probability function
  }
  \item{current}{
  starting value of algorithm
  }
  \item{C}{
  half-width of proposal interval
  }
  \item{iter}{
  number of iterations
  }
  \item{...}{
  other inputs needed in logpost function
  }
}

\value{
  \item{S}{vector of simulated values}
  \item{accept_rate}{acceptance rate of algorithm}
}
\author{
  Jim Albert
}

\examples{
lpost <- function(theta, s){
  dnorm(s$ybar, theta, s$se, log = TRUE) +
    dcauchy(theta, s$loc, s$scale, log = TRUE)
}
s <- list(ybar = 20,
          se = 0.4,
          loc = 10,
          scale = 2)
post <- metropolis(lpost, 10, 20, 100, s)
}
