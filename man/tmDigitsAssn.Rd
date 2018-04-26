\name{tmDigits<-}
\alias{tmDigits<-}
\title{
  Assignment function for the 'digits' attribute
}
\description{
  Assignment function for the 'digits' attribute of an object of
  class 'TableMonster' 
}
\usage{
  tmDigits(x) <- value
}
\arguments{
  \item{x}{
    An object of class 'TableMonster'
  }
  \item{value}{
    A numeric vector of length equal to the number of columns in the
    table specifying the desired number of digits. Enter '0' for
    character columns.
  }
}
\details{
  This is a required attribute for an object of class 'TableMonster'
}
\author{Grant Izmirlian}
\references{
  <https://www.youtube.com/watch?v=CM1TaNVnh58>
}
