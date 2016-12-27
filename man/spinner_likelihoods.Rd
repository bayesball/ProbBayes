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
  A matrix where each row corresponds to the outcome probabiliteis for one spinner.
}
\author{
  Jim Albert
}

\examples{
  spinner_likelihoods(list(c(2, 1, 1), c(1, 1, 1, 1)))
}
