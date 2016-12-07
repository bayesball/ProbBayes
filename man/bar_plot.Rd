\name{bar_plot}
\alias{bar_plot}

\title{
  Bar plot of numeric data
}
\description{
  Constructs frequency bar plot of some numeric data
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
}
