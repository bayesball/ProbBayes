\name{beta_prior_post}
\alias{beta_prior_post}

\title{
  Plot of Two Beta Curves
}
\description{
  Plot of Two Beta Curves
}
\usage{
  beta_prior_post(prior_shapes, post_shapes,
     label_1="Prior", label_2="Posterior")
}
\arguments{
  \item{prior_shapes}{
    vector of shape parameters of the beta prior
  }
  \item{post_shapes}{
    vector of shape parameters of the beta posterior
  }
  \item{label_1}{title for the first beta curve}
  \item{label_2}{title for the second beta curve}
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
