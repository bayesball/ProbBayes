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
    vector with components mean and sd of the normal prior
  }
  \item{data}{
    vector with components the sample mean and the standard error of the estimate
  }
  \item{teach}{
   logical variable indicating the form of the output
  }
}
\value{
  If teach = TRUE, returns data frame that displays the mean, precision, and standard deviation for the prior, data, and posterior.  If teach = FALSE, returns a vector with mean and standard deviation of the posterior.
}
\author{
  Jim Albert
}

\examples{
  prior <- c(100, 10)
  data <- c(110, 15)
  normal_update(prior, data)
  normal_update(prior, data, teach=TRUE)
}
