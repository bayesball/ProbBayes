\name{beta_prior_post}
\alias{beta_prior_post}

\title{
  Plot of Two Beta Curves
}
\description{
  Plot of Prior and Posterior Beta Curves
}
\usage{
  beta_prior_post(prior_shapes, post_shapes)
}
\arguments{
  \item{prior_shapes}{
    vector of shape parameters of the beta prior
  }
  \item{post_shapes}{
    vector of shape parameters of the beta posterior
  }
}

\value{
  ggplot2 object containing the graphical display.
}
\author{
  Jim Albert
}

\examples{
 prior_shapes <- c(4, 6)
 post_shapes <- c(19, 16)
 beta_prior_post(prior_shapes, post_shapes)
}
