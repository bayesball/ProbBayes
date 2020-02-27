\name{increasefont}
\alias{increasefont}

\title{
Increases font size of text
}
\description{
 Increases font size on all text in a ggplot2 graphic
}
\usage{
  increasefont(Size = 18)
}
\arguments{
  \item{Size}{
    font size of all textual elements in a ggplot2 graphic
  }
}
\value{ggplot2 theme code to increase the font size}
\author{
  Jim Albert
}

\examples{
df <- data.frame(p=c(.1, .3, .5, .7, .9),
                 Prior=rep(1/5, 5))
ggplot(df, aes(p, Prior)) +
geom_point() + increasefont()
}
