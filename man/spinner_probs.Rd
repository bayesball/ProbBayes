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
    vector of positive values for the spins 1, 2, ...
  }
}

\value{
  Dataframe with variables Region and Prob
}
\author{
  Jim Albert
}

\examples{
  regions <- c(2, 1, 1, 2)
  spinner_probs(regions)
}
