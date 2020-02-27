\name{normal_quantile}
\alias{normal_quantile}

\title{
  Displays a Quantile of a Normal Curve
}
\description{
  Displays a Quantile of a Normal Curve
}
\usage{
  normal_quantile(prob, normal_pars, Color = "orange")
}
\arguments{
  \item{prob}{
    probability value of interest
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
  normal_pars <- c(100, 10)
  prob <- 0.7
  normal_quantile(prob, normal_pars)
}
