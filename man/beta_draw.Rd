\name{beta_draw}
\alias{beta_draw}

\title{
  Draw a Beta Curve
}
\description{
  Draw a Beta Curve
}
\usage{
  beta_draw(shape_pars)
}
\arguments{
  \item{shape_pars}{
    vector of shape parameters of the beta curve
  }
  \item{...}{
    add a title to the graph if desired
  }
}

\value{
  Displays the beta curve
}
\author{
  Jim Albert
}

\examples{
  parameters <- c(2, 5)
  beta_draw(parameters)
}
