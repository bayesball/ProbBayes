\name{prob_plot}
\alias{prob_plot}

\title{
  Constructs a graph of a probability distribution
}
\description{
  Constructs a graph of a discrete probability distribution
}
\usage{
  prob_plot(d)
}
\arguments{
  \item{d}{
    data frame where the first two columns are the variable and associated probabilities
  }
}

\value{
  A ggplot2 object containing the plot display
}
\author{
  Jim Albert
}

\examples{
  probability_df <- data.frame(x=1:5,
         Probability=c(.1, .2, .3, .3, .1))
  prob_plot(probability_df)
}
