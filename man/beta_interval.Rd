\name{beta_interval}
\alias{beta_interval}

\title{
  Probability Interval for a Beta Curve
}
\description{
  Computes Probability Interval for a Beta Curve
}
\usage{
  beta_interval(prob, shape_par)
}
\arguments{
  \item{prob}{
    value of coverage probability
  }
  \item{shape_par}{
    vector of shape parameters of the beta curve
  }
}

\value{
  Computes and displays the "equal tails" interval that contains the beta curve with the specified probability
}
\author{
  Jim Albert
}

\examples{
  parameters <- c(2, 5)
  beta_interval(.5, parameters)
}
