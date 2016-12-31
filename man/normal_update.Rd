\name{normal_update}
\alias{normal_update}

\title{
  Updates a Normal Prior with Normal Data
}
\description{
  Finds the parameters of the normal posterior with normal data and a normal prior
}
\usage{
  normal_update(prior, data)
}
\arguments{
  \item{prior}{
    list with components mean and sd of the normal prior
  }
  \item{data}{
    list with components mean, the sample mean, and se, the standard error of the estimate
  }
}

\value{
  Displays the mean and standard deviation of the normal posterior
}
\author{
  Jim Albert
}

\examples{
  prior <- list(mean=100, sd=10)
  data <- list(mean=110, se=15)
  normal_update(prior, data)
}
