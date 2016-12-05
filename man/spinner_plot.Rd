\name{spinner_plot}
\alias{spinner_plot}

\title{
  Constructs a spinner
}
\description{
  Constructs a spinner with different regions
}
\usage{
  spinner_plot(regions, ...)
}
\arguments{
  \item{regions}{
    vector of integer areas for the spins 1, 2, ...
  }
  \item{...}{
    title for the plot
  }
}

\value{
  A ggplot2 object containing the spinner display
}
\author{
  Jim Albert
}

\examples{
  spinner_plot(c(1, 2, 2, 1), "My Spinner")
}
