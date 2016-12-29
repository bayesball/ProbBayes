\name{beta_area_quantile}
\alias{beta_area_quantile}

\title{
  Displays a Quantile of a Beta Curve
}
\description{
  Displays a Quantile of a Beta Curve
}
\usage{
  beta_area_quantile(prob, shape_par)
}
\arguments{
  \item{prob}{
    probability value of interest
  }
  \item{shape_par}{
    vector of shape parameters of the beta curve
  }
}

\value{
 Displays the quantile of a beta curve corresponding to probability prob
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
