\name{beta_prior_post}
\alias{beta_prior_post}

\title{
  Prior-Posterior Plot of a Proportion
}
\description{
  Prior-Posterior Plot of a Proportion
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
 Displays the beta prior and beta posterior curves
}
\author{
  Jim Albert
}

\examples{
 beta_prior_post(c(4, 6), c(19, 16))
}
