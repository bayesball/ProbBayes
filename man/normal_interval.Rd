\name{normal_interval}
\alias{normal_interval}

\title{
  Probability Interval for a Normal Curve
}
\description{
  Computes "equal-tails" probability interval for a normal curve
}
\usage{
  normal_interval(prob, normal_pars)
}
\arguments{
  \item{prob}{
    value of coverage probability
  }
  \item{normal_pars}{
    vector of mean and standard deviation of the normal curve
  }
}

\value{
  Computes and displays the interval that contains the normal curve with the specified probability
}
\author{
  Jim Albert
}

\examples{
  parameters <- c(2, 0.5)
  prob_content <- 0.5
  normal_interval(prob_content, parameters)
}
