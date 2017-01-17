\name{normal_quantile}
\alias{normal_quantile}

\title{
  Displays a Quantile of a Normal Curve
}
\description{
  Displays a Quantile of a Normal Curve
}
\usage{
  normal_quantile(prob, normal_pars)
}
\arguments{
  \item{prob}{
    probability value of interest
  }
  \item{normal_pars}{
    vector of mean and standard deviation of the normal curve
  }
}

\value{
 Displays the quantile of a normal curve corresponding to a specific probability value
}
\author{
  Jim Albert
}

\examples{
  parameters <- c(100, 10)
  prob_value <- 0.7
  normal_quantile(prob_value, parameters)
}
