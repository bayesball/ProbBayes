\name{normal_update}
\alias{normal_update}

\title{
  Updates a Normal Prior with Normal Data
}
\description{
  Finds the parameters of the normal posterior with normal data and a normal prior
}
\usage{
  normal_update(prior, data, teach=FALSE)
}
\arguments{
  \item{prior}{
    list with components mean and sd of the normal prior
  }
  \item{data}{
    list with components the sample mean and the standard error of the estimate
  \item{TEACH}{
   logical variable indicating the form of the output
  }

  }
}

\value{
  If TEACH = TRUE, returns data frame that displays the mean, precision, and standard deviation for the prior, data, and posterior.  If TEACH = FALSE, returns a vector with posterior mean and standard deviation.
}
\author{
  Jim Albert
}

\examples{
  prior <- list(mean=100, sd=10)
  data <- list(mean=110, se=15)
  normal_update(prior, data)
}
