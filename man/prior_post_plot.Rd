\name{prior_post_plot}
\alias{prior_post_plot}

\title{
  Graphs prior and posterior probabilities
}
\description{
  Graphs prior and posterior probabilities from a discrete Bayesian model
}
\usage{
  prior_post_plot(d, Color = "orange")
}
\arguments{
  \item{d}{
    data frame where the first column are the model values, and columns named Prior and Posterior
  }
  \item{Color}{
     fill color for the bars
  }
}

\value{
  Constructs a comparative plot of the prior and posterior distributions using separate panels.
}
\author{
  Jim Albert
}

\examples{
d <- data.frame(p=c(.1, .3, .5, .7, .9),
                 Prior=rep(1/5, 5))
y <- 5
n <- 10
d$Likelihood <- dbinom(y, prob=d$p, size=n)
d <- bayesian_crank(d)
prior_post_plot(d, "red")
}
