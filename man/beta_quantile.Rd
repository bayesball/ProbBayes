\name{beta_quantile}
\alias{beta_quantile}

\title{
  Displays a Quantile of a Beta Curve
}
\description{
  Displays a Quantile of a Beta Curve
}
\usage{
  beta_quantile(prob, shape_par, Color = "orange")
}
\arguments{
  \item{prob}{
    probability value of interest
  }
  \item{shape_par}{
    vector of shape parameters of the beta curve
  }
   \item{Color}{
    color of shading in the graph
  }
}

\value{
  ggplot2 object containing the graphical display.
}
\author{
  Jim Albert
}

\examples{
  # find the .50 quantile (the median)
  prob <- 0.5
  shape_par <- c(2, 5)
  beta_quantile(prob, shape_par)
  # find the .90 quantile (90th percentile)
  prob <- 0.9
  beta_quantile(prob, shape_par)
}
