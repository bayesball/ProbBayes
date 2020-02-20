\name{beta_area}
\alias{beta_area}

\title{
  Displays Areas Under a Beta Curve
}
\description{
  Computes and Displays Areas Under a Beta Curve
}
\usage{
  beta_area(lo, hi, shape_par, Color = "orange")
}
\arguments{
  \item{lo}{
    lower bound of interval
  }
  \item{hi}{
    upper bound of interval
  }
  \item{shape_par}{
    vector of shape parameters of the beta curve
  }
  \item{Color}{
    color of shading in the graph
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
  shape_par <- c(2, 5)
  beta_area(lo, hi, shape_par)
}
