\name{JAGS_script}
\alias{JAGS_script}

\title{
  JAGS Script for Common Models
}
\description{
  Model script for JAGS to fit a particular Bayesian model.  Currently the possible models are "beta_binomial", "hier_normal", "hier_trajectory", "normal", "regression", "regression_cond_means", and "trajectory".
}
\usage{
  JAGS_script(model)
}
\arguments{
  \item{model}{
    name of the model
  }
  }
\value{
A character string containing the model script
}
\keyword{datasets}

