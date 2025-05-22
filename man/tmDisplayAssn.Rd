\name{tmDisplay<-}
\alias{tmDisplay<-}
\title{
  Assignment function for the 'display' attribute
}
\description{
  Assignment function for the 'display' attribute of an object of class
  'TableMonster' 
}
\usage{
  tmDisplay(x) <- value
}
\arguments{
  \item{x}{
    An object of class 'TableMonster'
  }
  \item{value}{
    A vector of length exceeding the number of columns by 1 consiting of
    the format specifiers, '"d"' (for integers), '"f"', '"e"', '"E"',
    '"g"', '"G"', '"fg"' (for reals), or '"s"' (for strings).    
  }
}
\details{
  This attribute is optional and is only used when you want to specify
  the format type for each column as other than "g" (general format).
}
\author{Grant Izmirlian}
