\name{beta_prob}
\alias{beta_prob}

\title{
  Areas Under a Beta Curve
}
\description{
  Computes Areas Under a Beta Curve
}
\usage{
  beta_prob(lo, hi, shape_pars)
}
\arguments{
  \item{lo}{
    lower bound of interval
  }
  \item{hi}{
    upper bound of interval
  }
  \item{shape_pars}{
    vector of shape parameters of the beta curve
  }
}

\value{
  Probability that beta random variable falls between two values
}
\author{
  Jim Albert
}

\examples{
  lo <- .2
  hi <- .4
  parameters <- c(2, 5)
  beta_prob(lo, hi, parameters)
}
