\name{normal_draw}
\alias{normal_draw}

\title{
  Draws a Normal Curve
}
\description{
  Draws a Normal Curve
}
\usage{
  normal_draw(normal_pars, Color = "red")
}
\arguments{
  \item{normal_pars}{
    vector of mean and standard deviation of the normal curve
  }
  \item{Color}{
  color of line in plot
  }
}

\value{
  Displays the normal curve
}
\author{
  Jim Albert
}

\examples{
  normal_pars <- c(2, 1)
  normal_draw(normal_pars)
}
