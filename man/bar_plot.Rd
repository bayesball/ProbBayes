\name{bar_plot}
\alias{bar_plot}

\title{
  Bar plot of numeric or character data
}
\description{
  Constructs frequency bar plot of a vector of numeric data or a vector of character data
}
\usage{
  bar_plot(y, ...)
}
\arguments{
  \item{y}{
    vector of outcomes
  }
  \item{...}{
    title of the graph
  }
}

\value{
  A frequency bar graph of the outcomes
}
\author{
  Jim Albert
}

\examples{
  s <- spinner_data(c(1, 2, 2, 1), nsim=100)
  bar_plot(s, "Spinner Data")
  y <- c(rep("a", 10), rep("b", 5), rep("c", 8))
  bar_plot(y)
}
