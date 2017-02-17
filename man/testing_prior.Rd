\name{testing_prior}
\alias{testing_prior}

\title{
  Testing prior for two proportions
}
\description{
  Constructs a discrete distribution for two proportions under a testing or uniform hypotheses
}
\usage{
  testing_prior(lo=.1, hi=.9, n_values=9,
        pequal=0.5, uniform=FALSE)
}
\arguments{
  \item{lo}{
    minimum value of each proportion
  }
  \item{hi}{
    maximum value of each proportion
  }
  \item{n_values}{
    number of values of each proportion
  }
  \item{pequal}{
    probability of the equality of the two proportions
  }
  \item{uniform}{
    indicates if a uniform prior is desired
  }
}

\value{
  matrix of probabilities where the rows and columns are labeled by the values of the proportions
}
\author{
  Jim Albert
}

\examples{
  # testing prior where each proportion is
  # .1, .3, .5, .7, .9
  Prob <- testing_prior(.1, .9, 5)
  # uniform prior over same proportion values
  Prob <- testing_prior(.1, .9, 5, uniform=TRUE)
}
