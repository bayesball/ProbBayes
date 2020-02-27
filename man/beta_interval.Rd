\name{beta_interval}
\alias{beta_interval}

\title{
  Probability Interval for a Beta Curve
}
\description{
  Computes Probability Interval for a Beta Curve
}
\usage{
  beta_interval(prob, shape_par, Color = "orange")
}
\arguments{
  \item{prob}{
    value of coverage probability
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
  shape_par <- c(2, 5)
  beta_interval(.5, shape_par)
}
