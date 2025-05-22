\name{tmDisplay}
\alias{tmDisplay}
\title{
  Gets the 'display' attribute
}
\description{
  Gets the 'display' attribute of an object of class 'TableMonster'
}
\usage{
  tmDisplay(x)
}

\arguments{
  \item{x}{
    An object of class 'TableMonster'
  }
}
\details{
  This attribute is optional and is only used when you want to specify
  the format type for each column as other than "g" (general format).
}
\value{
  A vector of length exceeding the number of columns by 1 consiting of
  the format specifiers, '"d"' (for integers), '"f"', '"e"', '"E"',
  '"g"', '"G"', '"fg"' (for reals), or '"s"' (for strings).
}
\author{Grant Izmirlian}

