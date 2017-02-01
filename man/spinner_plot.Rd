\name{spinner_plot}
\alias{spinner_plot}

\title{
  Constructs a spinner
}
\description{
  Constructs a spinner with different regions
}
\usage{
  spinner_plot(probs, ...)
}
\arguments{
  \item{probs}{
    vector of probabilities for the spins 1, 2, ...
  }
  \item{...}{
    optional vector of values and title
  }
}

\value{
  A ggplot2 object containing the spinner display
}
\author{
  Jim Albert
}

\examples{
  probs <- rep(.2, 5)
  spinner_plot(probs,
         values=c("A", "B", "C", "D", "E"),
         title="My Spinner")
  # probs does not need to be normalized
  spinner_plot(c(1, 2, 1, 2))
}
