\name{normal_area}
\alias{normal_area}

\title{
  Displays Area Under a Normal Curve
}
\description{
  Computes and Displays Area Under a Normal Curve
}
\usage{
  normal_area(lo, hi, normal_pars, Color = "orange")
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
  \item{Color}{
  color of shading in plot
  }
}

\value{
  ggplot2 object containing the graphical display.
}
\author{
  Jim Albert
}

\examples{
  lo <- 10
  hi <- 20
  normal_pars <- c(25, 10)
  normal_area(lo, hi, normal_pars)
}
