\name{normal_interval}
\alias{normal_interval}

\title{
  Probability Interval for a Normal Curve
}
\description{
  Computes "equal-tails" probability interval for a normal curve
}
\usage{
  normal_interval(prob, normal_pars, Color = "orange")
}
\arguments{
  \item{prob}{
    value of coverage probability
  }
  \item{normal_pars}{
    vector of mean and standard deviation of the normal curve
  }
  \item{Color}{
  color of shading in plot
  }
}

\value{
  Computes and displays the interval that contains the normal curve with the specified probability
}
\author{
  Jim Albert
}

\examples{
  normal_pars <- c(2, 0.5)
  prob <- 0.5
  normal_interval(prob, normal_pars)
}
