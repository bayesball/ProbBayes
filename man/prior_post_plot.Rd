\name{prior_post_plot}
\alias{prior_post_plot}

\title{
  Graphs prior and posterior probabilities
}
\description{
  Graphs prior and posterior probabilities from a discrete Bayesian model
}
\usage{
  prior_post_plot(d,...)
}
\arguments{
  \item{d}{
    data frame where the first column are the model values, and columns named Prior and Posterior
  }
  \item{...}{
     gives an alternative label for the x axis
  }
}

\value{
  Constructs a comparative plot of the prior and posterior distributions using separate panels.
}
\author{
  Jim Albert
}

\examples{
df <- data.frame(p=c(.1, .3, .5, .7, .9),
                 Prior=rep(1/5, 5))
y <- 5
n <- 10
df$Likelihood <- dbinom(y, prob=df$p, size=n)
df <- bayesian_crank(df)
prior_post_plot(df, "Proportion")
}
