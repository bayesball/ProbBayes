\name{two_p_update}
\alias{two_p_update}

\title{
  Posterior updating of two proportions
}
\description{
  Computes posterior distribution of two proportions with a discrete prior
}
\usage{
  two_p_update(prior, s1f1, s2f2)
}
\arguments{
  \item{prior}{
    prior probability matrix where the rows and columns are labeled with the values of the proportions
  }
  \item{s1f1}{
  number of successes and number of failures from first sample
  }
   \item{s2f2}{
  number of successes and number of failures from second sample
  }
}

\value{
  posterior probability matrix
}
\author{
  Jim Albert
}

\examples{
  prior <- testing_prior()
  first_sample <- c(3, 10)
  second_sample <- c(8, 20)
  two_p_update(prior, first_sample, second_sample)
}
