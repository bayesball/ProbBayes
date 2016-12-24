\name{beta_data}
\alias{beta_data}

\title{
  Simulate random data from a beta curve
}
\description{
  Simulate random data from a beta curve
}
\usage{
  beta_data(shape_par, nsim=1000)
}
\arguments{
  \item{shape_par}{
    vector of shape parameters of the beta curve
  }
  \item{nsim}{
    number of simulations
  }
}

\value{
  A vector of random spins from the spinner
}
\author{
  Jim Albert
}

\examples{
  sp <- c(2, 1, 1, 2)
  spinner_data(sp, 10)
}
