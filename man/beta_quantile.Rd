\name{beta_quantile}
\alias{beta_quantile}

\title{
  Displays a Quantile of a Beta Curve
}
\description{
  Displays a Quantile of a Beta Curve
}
\usage{
  beta_quantile(prob, shape_par)
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
  prob <- 0.7
  parameters <- c(2, 5)
  beta_quantile(prob, parameters)
}
