\name{spinner_data}
\alias{spinner_data}

\title{
  Simulate random data from a spinner
}
\description{
  Simulate random data from a spinner
}
\usage{
  spinner_data(regions, nsim=1000)
}
\arguments{
  \item{regions}{
    vector of integer values for the spins 1, 2, ...
  }
  \item{nsim}{
    number of spins
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
