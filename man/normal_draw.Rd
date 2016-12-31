\name{normal_draw}
\alias{normal_draw}

\title{
  Draws a Normal Curve
}
\description{
  Draws a Normal Curve
}
\usage{
  normal_draw(normal_pars)
}
\arguments{
  \item{normal_pars}{
    vector of mean and standard deviation of the normal curve
  }
}

\value{
  Displays the normal curve
}
\author{
  Jim Albert
}

\examples{
  parameters <- c(2, 1)
  normal_draw(parameters)
}
