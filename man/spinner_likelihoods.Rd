\name{spinner_likelihoods}
\alias{spinner_likelihoods}

\title{
  Computes likelihood matrix for many spinners
}
\description{
  Computes likelihood matrix for many spinners
}
\usage{
  spinner_likelihoods(regions)
}
\arguments{
  \item{regions}{
    list of vectors of integer areas for the spins 1, 2, ...
  }
}

\value{
  A matrix where each row corresponds to the outcome probabilities for one spinner.
}
\author{
  Jim Albert
}

\examples{
  sp1 <- c(2, 1, 1)
  sp2 <- c(1, 1, 1, 1)
  spinner_likelihoods(list(sp1, sp2))
}
