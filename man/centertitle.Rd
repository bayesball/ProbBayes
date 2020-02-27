\name{centertitle}
\alias{centertitle}

\title{
Centers title in a ggplot2 graphic
}
\description{
Centers and increases font size of a ggplot2 graphic title
}
\usage{
centertitle(Color = "blue")
}
\arguments{
  \item{Color}{
    color of the text in the ggplot2 title
  }
}
\value{ggplot2 theme code to center the title}
\author{
  Jim Albert
}

\examples{
df <- data.frame(p=c(.1, .3, .5, .7, .9),
                 Prior=rep(1/5, 5))
ggplot(df, aes(p, Prior)) +
geom_point() +
ggtitle("My Prior") +
centertitle()
}
