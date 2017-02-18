\name{dsampling}
\alias{dsampling}

\title{
  Hypergeometric sampling density
}
\description{
  Hypergeometric sampling density
}
\usage{
  dsampling(sample_b, pop_N, pop_B, sample_n)
}
\arguments{
  \item{sample_b}{
    number of black balls in sample
  }
  \item{pop_N}{
    number of balls in population
  }
  \item{pop_B}{
    number of black balls in population
  }
  \item{sample_n}{
    number of balls in sample
  }
}

\value{
  Value of hypergeometric sampling probability
}
\author{
  Jim Albert
}

\examples{
  pop_N <- 10
  pop_B <- 4
  sample_n <- 3
  sample_b <- 2
  dsampling(sample_b, pop_N, pop_B, sample_n)
}
