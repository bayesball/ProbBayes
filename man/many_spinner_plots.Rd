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
  spinner_plot(c(1, 2, 2, 1), "My Spinner")
}
