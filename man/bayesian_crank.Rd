\name{bayesian_crank}
\alias{bayesian_crank}

\title{
  Computes Posterior Probabilities for Discrete Models
}
\description{
  Given a data table with columns Prior and Likelihood, computes posterior probabilities
}
\usage{
  bayesian_crank(d)
}
\arguments{
  \item{d}{
    data frame with columns Prior and Likelihood
  }
}

\value{
  data frame with new columns Product and Posterior
}
\author{
  Jim Albert
}

\examples{
  df <- data.frame(p=c(.1, .3, .5, .7, .9),
                   Prior=rep(1/5, 5))
  y <- 5
  n <- 10
  df$Likelihood <- dbinom(y, prob=df$p, size=n)
  df <- bayesian_crank(df)
}
