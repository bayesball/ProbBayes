\name{draw_two_p}
\alias{draw_two_p}

\title{
  Plot of Distribution of Two Proportions
}
\description{
  Constructs a graph of the probability distribution of two proportions
}
\usage{
  draw_two_p(prob_matrix, ...)
}
\arguments{
  \item{prob_matrix}{
    matrix of probabilities with labels the values of the proportions
  }
  \item{...}{
    other arguments such as the title of the plot
  }
}

\value{
  graphical display of the joint probability distribution
}
\author{
  Jim Albert
}

\examples{
  Prob <- testing_prior()
  draw_two_p(Prob, title="Testing Prior")
}
