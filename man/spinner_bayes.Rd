\name{spinner_bayes}
\alias{spinner_bayes}

\title{
  Implements Bayes' rule for a spinner problem
}
\description{
  Computes and plots the posterior distribution of spinners given a sequence of spins
}
\usage{
  spinner_bayes(list_regions,
                prior,
                data,
                plot=TRUE)
}
\arguments{
  \item{list_regions}{
    list of vectors of integer areas for the spins 1, 2, ...
  }
  \item{prior}{
    a vector containing the prior probabilities for the spinners
  }
  \item{data}{
    a vector containing the spin values where 1, 2, 3, ... are the possible spins
  }
  \item{plot}{
   if plot=TRUE, a comparative graph of the prior and posterior probabilities is displayed
  }
}

\value{
  A data frame with variables Spinner, Prior, Likelihood, Product, and Posterior
}
\author{
  Jim Albert
}

\examples{
  regions1 <- c(1, 1, 1)
  regions2 <- c(2, 1, 2, 1)
  data <- c(1, 1, 1, 2)
  spinner_bayes(list(regions1, regions2),
                prior=c(0.5, 0.5),
                data)
}
