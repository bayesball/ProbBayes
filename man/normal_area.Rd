\name{normal_area}
\alias{normal_area}

\title{
  Displays Area Under a Normal Curve
}
\description{
  Computes and Displays Area Under a Normal Curve
}
\usage{
  normal_area(lo, hi, normal_pars)
}
\arguments{
  \item{lo}{
    lower bound of interval
  }
  \item{hi}{
    upper bound of interval
  }
  \item{normal_pars}{
    vector of mean and standard deviation of the normal curve
  }
}

\value{
  Probability that normal random variable falls between two values
}
\author{
  Jim Albert
}

\examples{
  lo <- 10
  hi <- 20
  parameters <- c(25, 10)
  normal_area(lo, hi, parameters)
}
