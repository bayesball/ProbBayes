\name{many_spinner_plots}
\alias{many_spinner_plots}

\title{
  Graphs a collection of spinners
}
\description{
  Graphs a collection of spinners
}
\usage{
  many_spinner_plots(list_regions)
}
\arguments{
  \item{list_regions}{
    list of vectors of integer areas for the spins 1, 2, ...
  }
}

\value{
  A ggplot2 object containing the spinner displays
}
\author{
  Jim Albert
}

\examples{
  regions1 <- c(1, 1, 1)
  regions2 <- c(2, 1, 2, 1)
  many_spinner_plots(list(regions1, regions2))
}
