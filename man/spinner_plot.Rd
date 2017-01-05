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
  spinner_plot(rep(.2, 5),
         values=c("A", "B", "C", "D", "E"),
         title="My Spinner")
}
