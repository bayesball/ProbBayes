\name{many_normal_plots}
\alias{many_normal_plots}

\title{
  Graph of several normal curves
}
\description{
  Graph of several normal curves
}
\usage{
  many_normal_plots(list_normal_par)
}
\arguments{
  \item{list_normal_par}{
    list of vectors, where each vector is a mean and standard deviation for a normal distribution
  }
}

\value{
 Displays the normal curves on a single panel with labels
}
\author{
  Jim Albert
}

\examples{
 normal_parameters <- list(c(100, 15),
     c(110, 15), c(120, 15))
 many_normal_plots(normal_parameters)
}
