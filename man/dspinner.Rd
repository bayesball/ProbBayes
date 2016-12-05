\name{dspinner}
\alias{dspinner}

\title{
  Computes likelihoods for spinner outcomes
}
\description{
  Computes likelihoods for spinner outcomes
}
\usage{
  dspinner(x, Prob)
}
\arguments{
  \item{x}{
    vector of spinner observations
  }
  \item{Prob}{
    matrix of spinner probabilities where each row corresponds to a different spinner
  }
}

\value{
  column vector consisting of the likelihoods for the different spinners
}
\author{
  Jim Albert
}

\examples{
  Prob <- matrix(c(.25, .25, .25, .25,
                   .50, .125, .125, .5,
                   .25, .5, .25, 0), 3, 4, byrow=TRUE)
  obs <- c(1, 2, 1, 3, 4)
  dspinner(obs, Prob)
}
