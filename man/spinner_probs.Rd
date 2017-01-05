\name{spinner_probs}
\alias{spinner_probs}

\title{
  Display probability distribution for a spinner
}
\description{
  Display probability distribution for a spinner
}
\usage{
  spinner_probs(regions)
}
\arguments{
  \item{regions}{
    vector of integer values for the spins 1, 2, ...
  }
}

\value{
  Data frame with variables Spin and Prob
}
\author{
  Jim Albert
}

\examples{
  sp <- c(2, 1, 1, 2)
  spinner_probs(sp)
}
