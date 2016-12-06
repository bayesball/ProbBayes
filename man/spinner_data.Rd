\name{spinner_data}
\alias{spinner_data}

\title{
  Simulates data from a spinner
}
\description{
  Simulates data from a spinner with different regions
}
\usage{
  spinner_data(regions, nsim=1000)
}
\arguments{
  \item{regions}{
    vector of integer areas for the spins 1, 2, ...
  }
  \item{nsim}{
    number of simulated spins
  }
}

\value{
  A vector of simulated spins from the spinner
}
\author{
  Jim Albert
}

\examples{
  spinner_data(c(1, 2, 2, 1), nsim=20)
}
