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
 Displays the two beta curves on the same panel
}
\author{
  Jim Albert
}

\examples{
 beta_prior_post(c(4, 6), c(19, 16))
}
